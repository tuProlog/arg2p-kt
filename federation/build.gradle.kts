plugins {
    id(
        libs.plugins.ktMpp.mavenPublish
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
                .file("it/unibo/tuprolog/argumentation/federation/Version.kt")
                .asFile
                .apply { parentFile.mkdirs() }
                .writeText(
                    "package it.unibo.tuprolog.argumentation.federation\n\n" +
                        "/** The version of arg2p this module was built with */\n" +
                        "internal const val ARG2P_VERSION: String = \"$version\"\n",
                )
        }
    }

kotlin {
    sourceSets {
        commonMain {
            kotlin.srcDir(generateVersion)
            dependencies {
                api(project(":parallel"))
            }
        }
        // (the scenarios the parallel solvers run, in this module and in parallel)
        commonTest {
            kotlin.srcDir("../parallel/src/sharedTest/kotlin")
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
