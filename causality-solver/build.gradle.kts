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

        commonTest {
            dependencies {
                implementation(libs.tuprolog.test.solve)
                // tuProlog's own solver, to check the causality module on an engine other than the one of arg2p
                implementation(libs.tuprolog.solve.prolog)
            }
        }
    }
}
