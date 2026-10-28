import org.jetbrains.kotlin.gradle.targets.jvm.tasks.KotlinJvmTest

val jvmStackSize: String = property("jvmStackSize").toString()
val jvmMaxHeapSize: String = property("jvmMaxHeapSize").toString()

plugins {
    id(
        libs.plugins.ktMpp.mavenPublish
            .get()
            .pluginId,
    )
    id(
        libs.plugins.ktMpp.npmPublish
            .get()
            .pluginId,
    )
}

// The version of arg2p as a constant of the module: federated sites check that they are of the same one
val generateVersion =
    tasks.register("generateVersion") {
        val version = project.version.toString()
        val directory = layout.buildDirectory.dir("generated/version/kotlin")
        inputs.property("version", version)
        outputs.dir(directory)
        doLast {
            directory
                .get()
                .file("it/unibo/tuprolog/argumentation/parallel/federation/Version.kt")
                .asFile
                .apply { parentFile.mkdirs() }
                .writeText(
                    "package it.unibo.tuprolog.argumentation.parallel.federation\n\n" +
                        "/** The version of arg2p this module was built with */\n" +
                        "internal const val ARG2P_VERSION: String = \"$version\"\n",
                )
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
    }

    sourceSets {
        commonMain {
            kotlin.srcDir(generateVersion)
            dependencies {
                api(project(":core"))
            }
        }
        jvmMain {
            dependencies {
                // the links over WebSockets
                implementation(libs.java.websocket)
            }
        }
        jsMain {
            dependencies {
                // the WebSocket server of Node.js (the clients are those of the runtime)
                implementation(npm("ws", libs.versions.ws.get()))
            }
        }
    }
}

tasks.withType<KotlinJvmTest> {
    maxHeapSize = jvmMaxHeapSize
    jvmArgs("-Xss$jvmStackSize")
}

// The description of the module in the Kotlin & JS reference of the documentation site
extensions.configure<org.jetbrains.dokka.gradle.DokkaExtension> {
    dokkaSourceSets.configureEach { includes.from("Module.md") }
}
