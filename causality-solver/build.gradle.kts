import org.jetbrains.kotlin.gradle.targets.jvm.tasks.KotlinJvmTest

val jvmStackSize: String by project
val jvmMaxHeapSize: String by project

plugins {
    id(libs.plugins.ktMpp.mavenPublish.get().pluginId)
    id(libs.plugins.ktMpp.npmPublish.get().pluginId)
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
            dependencies {
                implementation(project(":core"))

                implementation(libs.tuprolog.dsl.solve)
                implementation(libs.tuprolog.solve.classic)
                implementation(libs.tuprolog.parser.theory)
            }
        }

        commonTest {
            dependencies {
                implementation(libs.tuprolog.test.solve)
            }
        }
    }
}

tasks.withType<KotlinJvmTest> {
    maxHeapSize = jvmMaxHeapSize
    jvmArgs("-Xss$jvmStackSize")
}
