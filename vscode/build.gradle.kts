plugins {
    // the JDKs of the toolchains, for the Java runtime of the extension
    `jvm-toolchains`
}

// The extension of VS Code (see README.md): the language arg2p, and the solver of a theory, from the language server of
// arg2p (:lsp), which it carries as a jar, with a Java runtime of its own in a package for a platform.

val jvmVersion: String = libs.versions.jvm.get()

// npm, as the build runs it (npm.cmd on Windows)
val npm = if (System.getProperty("os.name").lowercase().contains("windows")) "npm.cmd" else "npm"

val vscodeServer by tasks.registering(Copy::class) {
    group = "vscode"
    description = "Copies the language server into the extension (server/arg2p-lsp.jar)"
    from(project(":lsp").tasks.named("shadowJar"))
    into(layout.projectDirectory.dir("server"))
    rename { "arg2p-lsp.jar" }
}

// The modules of the Java runtime the server needs: those of arg2p (as the Python package bundles them), and java.sql
// for the JSON of the protocol
val runtimeModules =
    listOf(
        "java.base",
        "java.logging",
        "java.management",
        "java.naming",
        "java.net.http",
        "java.sql",
        "java.xml",
        "jdk.crypto.ec",
        "jdk.unsupported",
    )

val vscodeRuntime by tasks.registering(Exec::class) {
    group = "vscode"
    description = "Makes the Java runtime the extension bundles, for this platform (runtime/)"
    val jdk = javaToolchains.compilerFor { languageVersion.set(JavaLanguageVersion.of(jvmVersion)) }
    val output = layout.projectDirectory.dir("runtime")
    outputs.dir(output)
    doFirst { delete(output) }
    executable(jdk.map { it.metadata.installationPath.file("bin/jlink").asFile.absolutePath }.get())
    args(
        "--add-modules",
        runtimeModules.joinToString(","),
        "--strip-debug",
        "--no-header-files",
        "--no-man-pages",
        "--compress",
        "zip-6",
        "--output",
        output.asFile.absolutePath,
    )
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
