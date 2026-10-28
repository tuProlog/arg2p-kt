import io.github.gciatto.kt.mpp.Plugins
import io.github.gciatto.kt.mpp.helpers.ProjectType
import org.gradle.jvm.toolchain.JavaLanguageVersion
import org.gradle.jvm.toolchain.JavaToolchainService
import org.jetbrains.kotlin.gradle.targets.js.ir.KotlinJsIrLink

plugins {
    alias(libs.plugins.ktMpp.helper)
    alias(libs.plugins.ktMpp.mavenPublish)
    alias(libs.plugins.ktMpp.npmPublish)
    alias(libs.plugins.ktMpp.multiplatform)
    alias(libs.plugins.gitSemVer)
    alias(libs.plugins.kover) apply false
    // the JDKs of the toolchains, for the Java runtime of arg2p (javaRuntime)
    `jvm-toolchains`
}

group = "it.unibo.tuprolog.argumentation"

val jvmVersion: String = libs.versions.jvm.get()

// A property reference built as a standalone statement, whose value is never used
val deadPropertyReference = Regex("""(?m)^([ \t]*)getPropertyCallableRef\([^;\n]*\);[ \t]*$""")

// A static import of a module of Node.js alone, `import { writeSync as writeSync, ... } from 'fs';` (kotlinx-io declares
// fs a @JsModule): an ES module resolves its imports as it loads, so in a browser (or a bundle for one) the whole of
// arg2p would fail to load
val nodeOnlyImport = Regex("""(?m)^import \{([^}]*)\} from '(fs|os|path)';""")

// The import as a lookup in Node.js, when there is one: elsewhere what uses the module fails when it is used
fun lazyNodeImport(match: MatchResult): String {
    val bindings =
        match.groupValues[1]
            .split(',')
            .map { it.trim() }
            .filter { it.isNotEmpty() }
            .joinToString(", ") { binding ->
                val (name, alias) = binding.split(" as ").map { it.trim() }.let { it.first() to it.last() }
                if (name == alias) name else "$name: $alias"
            }
    return "const { $bindings } = globalThis.process?.getBuiltinModule?.('${match.groupValues[2]}') ?? {};"
}

// A call of require in the code of 2p-kt, `require('os')` or `eval('require')('url')`, for a module of Node.js or one
// it depends on (sync-request): an ES module has no require, and bundlers take the call for an import to bundle
val requireCall = Regex("""eval\('require'\)|(?<![\w$.'"])require(?=\(')""")

// require, as Node.js gives it to an ES module: elsewhere, what asks for a module fails when it does
val nodeRequire =
    "const __nodeRequire = globalThis.process?.getBuiltinModule?.('module')?.createRequire(import.meta.url) ?? " +
        "(name => { throw new Error(`\${name} is available in Node.js alone`); });\n"

// The calls of require going through Node.js (nodeRequire), but those in a string (code run elsewhere, as a worker)
fun nodeRequires(code: String): String {
    var replaced = false
    val patched =
        code.lines().joinToString("\n") { line ->
            requireCall.replace(line) { match ->
                val before = line.substring(0, match.range.first)
                val quoted = before.count { it == '"' } % 2 == 1 || before.count { it == '`' } % 2 == 1
                if (quoted) {
                    match.value
                } else {
                    replaced = true
                    "__nodeRequire"
                }
            }
        }
    return if (replaced) nodeRequire + patched else code
}

allprojects {
    repositories {
        google()
        mavenCentral()
    }

    // Gradle itself runs on the JDK set in gradle/gradle-daemon-jvm.properties,
    // while artifacts target (and tests run on) the JVM version from the catalog, unless -PtestJvm is provided
    tasks.withType<Test>().configureEach {
        javaLauncher.set(
            project.extensions.getByType<JavaToolchainService>().launcherFor {
                languageVersion.set(JavaLanguageVersion.of(project.findProperty("testJvm")?.toString() ?: jvmVersion))
            },
        )
    }
    tasks.withType<JavaCompile>().configureEach {
        options.release.set(jvmVersion.toInt())
    }

    // Kotlin/JS re-creates (and then discards) a property reference on every access to a delegated property,
    // e.g. `by lazy` (see KT-78124): in 2p-kt terms this dominates JS execution time, so dead references are removed.
    // The modules of Node.js are taken as the code runs, for it to load in browsers and bundles too (see nodeOnlyImport
    // and requireCall).
    tasks.withType<KotlinJsIrLink>().configureEach {
        doLast {
            destinationDirectory
                .get()
                .asFileTree
                .matching { include("**/*.js", "**/*.mjs") }
                .forEach { file ->
                    val original = file.readText()
                    val patched =
                        deadPropertyReference
                            .replace(original) { "${it.groupValues[1]}/* dead property reference removed */" }
                            .let { nodeOnlyImport.replace(it, ::lazyNodeImport) }
                            .let(::nodeRequires)
                    if (patched != original) file.writeText(patched)
                }
        }
    }
}

gitSemVer {
    buildMetadataSeparator.set("-")
}

multiProjectHelper {
    defaultProjectType = ProjectType.JS

    ktProjects(rootProject.path, ":core", ":causality-solver", ":parallel-solver")
    jvmProjects(":actor-solver", ":lsp")
    // jsProjects()
    otherProjects(":doc", ":python", ":vscode")

    val baseProjectTemplate =
        buildSet {
            add(Plugins.documentation)
            add(Plugins.versions)
            add(Plugins.linter)
        }

    ktProjectTemplate =
        buildSet {
            addAll(baseProjectTemplate)
            add(Plugins.multiplatform)
        }

    jvmProjectTemplate =
        buildSet {
            addAll(baseProjectTemplate)
            add(Plugins.jvmOnly)
        }

    jsProjectTemplate =
        buildSet {
            addAll(baseProjectTemplate)
            add(Plugins.jsOnly)
        }

    otherProjectTemplate =
        buildSet {
            add(Plugins.versions)
        }

    applyProjectTemplates()
}

// The version of arg2p (Arg2pLibrary.version()): the project version, the released one when CI builds a release
val generateVersion =
    tasks.register("generateVersion") {
        val version = project.version.toString()
        val output = layout.buildDirectory.dir("generated/version/kotlin")
        inputs.property("version", version)
        outputs.dir(output)
        doLast {
            output
                .get()
                .file("it/unibo/tuprolog/argumentation/Arg2pVersion.kt")
                .asFile
                .apply {
                    parentFile.mkdirs()
                    writeText(
                        "package it.unibo.tuprolog.argumentation\n\n" +
                            "// Generated by the build (generateVersion): the version of arg2p\n" +
                            "internal const val ARG2P_VERSION = \"$version\"\n",
                    )
                }
        }
    }

kotlin {

    js {
        compilations.all {
            compileTaskProvider.configure {
                compilerOptions.freeCompilerArgs.add("-Xir-minimized-member-names=false")
            }
        }
        binaries.library()
        // the declarations of the exported API for TypeScript (.d.ts), next to the JavaScript
        generateTypeScriptDefinitions()
        // ES modules, as bundlers, Node.js and Deno import them
        useEsModules()
    }

    sourceSets {
        commonMain {
            kotlin.srcDir(generateVersion)
            dependencies {
                api(project(":core"))
                api(project(":causality-solver"))
                api(project(":parallel-solver"))
            }
        }
    }
}

// The tests of the API run the solvers of arg2p, as those of the modules
val jvmStackSize: String = property("jvmStackSize").toString()
val jvmMaxHeapSize: String = property("jvmMaxHeapSize").toString()

tasks.withType<org.jetbrains.kotlin.gradle.targets.jvm.tasks.KotlinJvmTest> {
    maxHeapSize = jvmMaxHeapSize
    jvmArgs("-Xss$jvmStackSize")
}

// The modules of the Java runtime of arg2p: jdeps on its jars and those of its servers (:lsp), with those the
// WebSockets and their TLS need, and java.sql for the JSON of the protocol
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

// The Java runtime of arg2p, for this platform: the one the Python package and the extension of VS Code bundle (see
// pythonRuntime and vscodeRuntime)
tasks.register<Exec>("javaRuntime") {
    group = "build"
    description = "Makes the Java runtime of arg2p for this platform (build/runtime)"
    val jdk = javaToolchains.compilerFor { languageVersion.set(JavaLanguageVersion.of(jvmVersion)) }
    val output = layout.buildDirectory.dir("runtime")
    inputs.property("jvm", jvmVersion)
    inputs.property("modules", runtimeModules)
    outputs.dir(output)
    doFirst { delete(output) }
    executable(
        jdk
            .map {
                it.metadata.installationPath
                    .file("bin/jlink")
                    .asFile.absolutePath
            }.get(),
    )
    args(
        "--add-modules",
        runtimeModules.joinToString(","),
        "--strip-debug",
        "--no-header-files",
        "--no-man-pages",
        "--compress",
        "zip-6",
        "--output",
        output.get().asFile.absolutePath,
    )
}

// The npm package of arg2p, the only one (the modules it is made of are published on Maven alone): an ES module (see
// useEsModules) with every declaration of the API, and its declarations for TypeScript, as package.json tells Node.js,
// bundlers and TypeScript. Set once the project is evaluated, as kt-mpp configures the package after this. Its
// repository is the one its provenance names: npm refuses the package otherwise.
afterEvaluate {
    npmPublish {
        packages.configureEach {
            packageJson {
                main.set("arg2p.mjs")
                types.set("arg2p.d.mts")
                this["type"] = "module"
                this["exports"] =
                    mapOf(
                        "." to mapOf("types" to "./arg2p.d.mts", "import" to "./arg2p.mjs"),
                        "./package.json" to "./package.json",
                    )
                description.set(property("projectDescription").toString())
                homepage.set(property("projectHomepage").toString())
                license.set(property("projectLicense").toString())
                repository {
                    type.set("git")
                    url.set("git+${property("scmUrl")}.git")
                }
                bugs {
                    url.set(property("issuesUrl").toString())
                    email.set(property("issuesEmail").toString())
                }
                author {
                    name.set(property("developerPisanoName").toString())
                    email.set(property("developerPisanoEmail").toString())
                    url.set(property("developerPisanoUrl").toString())
                }
            }
        }
    }
}

// The coverage of the Kotlin code by the tests on the JVM (Kover), with -Pcoverage alone, as it instruments them: the
// code of core, of the causality and the parallel solver and of the API (commonMain is the code of JavaScript as
// well), in one report (build/reports/kover), and its total for a badge (koverBadge)
val coverage = findProperty("coverage") != null
val covered = listOf(":core", ":causality-solver", ":parallel-solver")

if (coverage) {
    apply(plugin = "org.jetbrains.kotlinx.kover")
    covered.forEach { project(it).apply(plugin = "org.jetbrains.kotlinx.kover") }
    dependencies { covered.forEach { "kover"(project(it)) } }

    tasks.register("koverBadge") {
        group = "verification"
        description = "Writes the coverage of the lines for a badge, as shields.io reads it (build/reports/kover/badge.json)"
        dependsOn("koverXmlReport")
        val report = layout.buildDirectory.file("reports/kover/report.xml")
        val badge = layout.buildDirectory.file("reports/kover/badge.json")
        inputs.file(report)
        outputs.file(badge)
        doLast {
            // the counter of the lines of the whole report, the last one
            val line = Regex("""<counter type="LINE" missed="(\d+)" covered="(\d+)"/>""").findAll(report.get().asFile.readText()).last()
            val (missed, hit) = line.destructured
            val share = 100 * hit.toInt() / (hit.toInt() + missed.toInt())
            val color =
                if (share >= 90) {
                    "brightgreen"
                } else if (share >= 75) {
                    "green"
                } else if (share >= 60) {
                    "yellow"
                } else {
                    "orange"
                }
            badge.get().asFile.writeText(
                "{\"schemaVersion\": 1, \"label\": \"Kotlin coverage\", \"message\": \"$share%\", \"color\": \"$color\"}\n",
            )
        }
    }
}

// The Kotlin & JS reference in the style of the documentation site
allprojects {
    plugins.withId("org.jetbrains.dokka") {
        extensions.configure<org.jetbrains.dokka.gradle.DokkaExtension> {
            pluginsConfiguration.withType<org.jetbrains.dokka.gradle.engine.plugins.DokkaHtmlPluginParameters>().configureEach {
                customStyleSheets.from(rootProject.file("doc/dokka/site.css"))
                footerMessage.set("arg2p")
            }
        }
    }
}

// The Kotlin & JS reference of the documentation site: the npm package with the libraries it is made of
dependencies {
    "dokka"(project(":core"))
    "dokka"(project(":causality-solver"))
    "dokka"(project(":parallel-solver"))
}

// project.findProperty("nodeVersion")?.toString()?.takeIf { it.isNotBlank() }?.let {
//    nodeVersion(it)
//    log("override NodeJS version: $it", LogLevel.LIFECYCLE)
// }

afterEvaluate {
    subprojects {
        version = rootProject.version
    }
}
