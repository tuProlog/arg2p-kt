import org.jetbrains.kotlin.gradle.targets.jvm.tasks.KotlinJvmTest

val jvmStackSize: String = property("jvmStackSize").toString()
val jvmMaxHeapSize: String = property("jvmMaxHeapSize").toString()

plugins {
    id(
        libs.plugins.ktMpp.mavenPublish
            .get()
            .pluginId,
    )
}

kotlin {

    js {
        compilations.all {
            compileTaskProvider.configure {
                compilerOptions.freeCompilerArgs.add("-Xir-minimized-member-names=false")
            }
        }
        binaries.library()
        // ES modules, as the npm package of arg2p (the aggregate project, which alone is published on npm)
        useEsModules()
    }

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

tasks.withType<KotlinJvmTest> {
    maxHeapSize = jvmMaxHeapSize
    jvmArgs("-Xss$jvmStackSize")
}

// The description of the module in the Kotlin & JS reference of the documentation site
extensions.configure<org.jetbrains.dokka.gradle.DokkaExtension> {
    dokkaSourceSets.configureEach { includes.from("Module.md") }
}
