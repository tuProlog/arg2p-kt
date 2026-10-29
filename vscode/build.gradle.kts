// The extension of VS Code (see README.md): the language arg2p, and the solver of a theory, from the language server of
// arg2p (:lsp), which it carries as a jar, with a Java runtime of its own in a package for a platform.

val os = System.getProperty("os.name").lowercase()

// npm, as the build runs it (npm.cmd on Windows)
val npm = if ("windows" in os) "npm.cmd" else "npm"

val vscodeServer by tasks.registering(Copy::class) {
    group = "vscode"
    description = "Copies the language server into the extension (server/arg2p-lsp.jar)"
    from(project(":lsp").tasks.named("shadowJar"))
    into(layout.projectDirectory.dir("server"))
    rename { "arg2p-lsp.jar" }
}

val runtime = layout.projectDirectory.dir("runtime")

val vscodeRuntime by tasks.registering(Sync::class) {
    group = "vscode"
    description = "Copies the Java runtime of arg2p for this platform into the extension (runtime/)"
    from(rootProject.tasks.named("javaRuntime"))
    into(runtime)
}

val vscodeInstall by tasks.registering(Exec::class) {
    group = "vscode"
    description = "Installs the dependencies of the extension"
    workingDir = projectDir
    inputs.files("package.json", "package-lock.json")
    // the lockfile npm writes into node_modules as it installs: the whole of node_modules is too large to check
    outputs.file("node_modules/.package-lock.json")
    commandLine(npm, "ci")
}

val vscodeTest by tasks.registering(Exec::class) {
    group = "vscode"
    description =
        "Tests the extension: its types, its bundles, its logic and the language server through JSON-RPC, and the " +
        "extension in VS Code (which needs a display: xvfb-run on Linux, say), as CI does"
    dependsOn(vscodeInstall, vscodeServer)
    workingDir = projectDir
    commandLine(npm, "run", "verify")
}

// The version of arg2p as the Marketplace takes it (major.minor.patch): a development version as the release it leads to
val vscodeVersion = provider { rootProject.version.toString().substringBefore('-') }

// The platform of this machine as VS Code names it (win32-x64, darwin-arm64, ...): the one of the runtime made here
val host: String =
    run {
        val system = if ("windows" in os) "win32" else if ("mac" in os) "darwin" else "linux"
        val arch =
            when (val name = System.getProperty("os.arch")) {
                "amd64", "x86_64" -> "x64"
                "aarch64" -> "arm64"
                else -> name
            }
        "$system-$arch"
    }

val vscodePackage by tasks.registering(Exec::class) {
    group = "vscode"
    description =
        "Packages the extension (arg2p-<version>.vsix): with -Ptarget=win32-x64 (say), for that platform alone, with the " +
        "Java runtime of this one (which must be it); without, for any (with no runtime/)"
    val target = findProperty("target")?.toString()
    dependsOn(listOfNotNull(vscodeInstall, vscodeServer, target?.let { vscodeRuntime }))
    workingDir = projectDir
    val version = vscodeVersion
    doFirst {
        // the runtime of this platform alone, in its package alone: .vscodeignore takes runtime/ in
        if (target == null) {
            check(!runtime.asFile.exists()) { "A package for any platform with the Java runtime of this one: delete ${runtime.asFile}" }
        } else {
            check(target == host) { "A package for $target with the Java runtime of $host: package it on $target" }
        }
        commandLine(
            listOfNotNull(npm, "run", "package", "--", version.get(), "--no-update-package-json", target?.let { "--target" }, target),
        )
    }
}
