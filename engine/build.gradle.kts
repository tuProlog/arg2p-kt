// The machine solving the theories of arg2p, on the interfaces of 2P-Kt. No package of its own yet: core compiles these
// sources into its own, for what is published to stay what it was (see core/build.gradle.kts); this project compiles
// them alone, for the machine to know nothing of arg2p.
plugins {
    id(
        libs.plugins.ktMpp.multiplatform
            .get()
            .pluginId,
    )
}

kotlin {
    sourceSets {
        commonMain {
            dependencies {
                api(libs.tuprolog.dsl.solve)
                api(libs.tuprolog.parser.theory)
            }
        }
    }
}

// The sources belong to core in the IDE, as a directory belongs to one module there
if (System.getProperty("idea.sync.active") == "true") {
    kotlin.sourceSets.configureEach { kotlin.setSrcDirs(emptyList<Any>()) }
}

// Compiled alone for the check that it knows nothing of arg2p, once: its tests are those of core, which runs it
tasks.matching { it.name.startsWith("js") || it.name.startsWith("compileKotlinJs") || it.name.contains("Dokka") }.configureEach {
    enabled =
        false
}
