// The user interface of arg2p as web components (see README.md), the npm package @tuprolog/arg2p-ui: built and tested against
// the npm package of arg2p of this commit (assembleJsPackage), which the extension of VS Code bundles and the pages of the
// language server load; released with arg2p, at its version.

val os = System.getProperty("os.name").lowercase()

// npm, as the build runs it (npm.cmd on Windows)
val npm = if ("windows" in os) "npm.cmd" else "npm"

val uiInstall by tasks.registering(Exec::class) {
    group = "ui"
    description = "Installs the dependencies of the components, arg2p as this commit builds it (see package.json)"
    dependsOn(rootProject.tasks.named("assembleJsPackage"))
    workingDir = projectDir
    inputs.files("package.json", "package-lock.json")
    inputs.dir(rootProject.layout.buildDirectory.dir("packages/js"))
    // the lockfile npm writes into node_modules as it installs: the whole of node_modules is too large to check
    outputs.file("node_modules/.package-lock.json")
    commandLine(npm, "ci")
}

val uiBuild by tasks.registering(Exec::class) {
    group = "ui"
    description = "Builds the components (dist/): the modules and their types, and the solver in one file for the pages"
    dependsOn(uiInstall)
    workingDir = projectDir
    inputs.dir("src")
    inputs.files("package.json", "tsconfig.json", "tsconfig.build.json", "vite.bundle.config.ts")
    outputs.dir("dist")
    commandLine(npm, "run", "build")
}

val uiTest by tasks.registering(Exec::class) {
    group = "ui"
    description = "Checks the types of the components and runs their tests"
    dependsOn(uiInstall)
    workingDir = projectDir
    commandLine(npm, "test")
}

val uiCheck by tasks.registering(Exec::class) {
    group = "ui"
    description = "Checks the types of the components and of their tests"
    dependsOn(uiInstall)
    workingDir = projectDir
    commandLine(npm, "run", "check")
}
