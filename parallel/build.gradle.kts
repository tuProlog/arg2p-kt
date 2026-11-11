plugins {
    id(
        libs.plugins.ktMpp.mavenPublish
            .get()
            .pluginId,
    )
}

kotlin {
    sourceSets {
        commonMain {
            dependencies {
                api(project(":core"))
            }
        }
        // (the scenarios the parallel solvers run, in this module and in federation)
        commonTest {
            kotlin.srcDir("src/sharedTest/kotlin")
        }
    }
}
