// The extension of VS Code (see README.md): the language arg2p, and the solver of a theory, from the language server of
// arg2p (:lsp), which it carries as a jar, with a Java runtime of its own in a package for a platform.

// npm, as the build runs it (npm.cmd on Windows)
val npm = if (System.getProperty("os.name").lowercase().contains("windows")) "npm.cmd" else "npm"

val vscodeServer by tasks.registering(Copy::class) {
    group = "vscode"
    description = "Copies the language server into the extension (server/arg2p-lsp.jar)"
    from(project(":lsp").tasks.named("shadowJar"))
    into(layout.projectDirectory.dir("server"))
    rename { "arg2p-lsp.jar" }
}

val vscodeRuntime by tasks.registering(Sync::class) {
    group = "vscode"
    description = "Copies the Java runtime of arg2p for this platform into the extension (runtime/)"
    from(rootProject.tasks.named("javaRuntime"))
    into(layout.projectDirectory.dir("runtime"))
}

val vscodeInstall by tasks.registering(Exec::class) {
    group = "vscode"
    description = "Installs the dependencies of the extension"
    workingDir = projectDir
    commandLine(npm, "ci")
}

val vscodeTest by tasks.registering(Exec::class) {
    group = "vscode"
    description = "Tests the extension (and the language server through JSON-RPC)"
    dependsOn(vscodeInstall, vscodeServer)
    workingDir = projectDir
    commandLine(npm, "test")
}

// The version of arg2p as the Marketplace takes it (major.minor.patch): a development version as the release it leads to
val vscodeVersion = provider { rootProject.version.toString().substringBefore('-') }

val vscodePackage by tasks.registering(Exec::class) {
    group = "vscode"
    description =
        "Packages the extension (arg2p-<version>.vsix): with -Ptarget=win32-x64 (say) and the runtime of vscodeRuntime, " +
        "for that platform alone"
    dependsOn(vscodeInstall, vscodeServer)
    workingDir = projectDir
    val target = findProperty("target")?.toString()
    val version = vscodeVersion
    doFirst {
        commandLine(
            listOfNotNull(npm, "run", "package", "--", version.get(), "--no-update-package-json", target?.let { "--target" }, target),
        )
    }
}
