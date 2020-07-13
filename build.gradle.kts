plugins {
    kotlin("multiplatform") version "1.3.72"
}

repositories {
    mavenCentral()
    jcenter()
}

val javaVersion: String by project
val ktFreeCompilerArgsJvm: String by project

group = "it.unibo.tuprolog.argumentation"
version = "0.1.0"

subprojects {

    group = rootProject.group
    version = rootProject.version

    repositories.addAll(rootProject.repositories)

    apply(plugin = "org.jetbrains.kotlin.multiplatform")
    apply(plugin = "maven-publish")

    kotlin {
        sourceSets {
            val commonMain by getting {
                dependencies {
                    implementation(kotlin("stdlib-common"))
                }
            }

            val commonTest by getting {
                dependencies {
                    implementation(kotlin("test-common"))
                    implementation(kotlin("test-annotations-common"))
                }
            }

            jvm {

                compilations["main"].defaultSourceSet {
                    dependencies {
                        implementation(kotlin("stdlib-jdk8"))
                    }
                }

                compilations["test"].defaultSourceSet {
                    dependencies {
                        implementation(kotlin("test-junit"))
                    }
                }

                mavenPublication {
                    artifactId = project.name + "-jvm"
                }
            }

            js {
                nodejs()

                compilations["main"].defaultSourceSet {
                    dependencies {
                        implementation(kotlin("stdlib-js"))
                    }
                }

                compilations["test"].defaultSourceSet {
                    dependencies {
                        implementation(kotlin("test-js"))
                    }
                }

                mavenPublication {
                    artifactId = project.name + "-js"
                }
            }
        }
    }

    tasks.withType<org.jetbrains.kotlin.gradle.dsl.KotlinJsCompile> {
        kotlinOptions {
            moduleKind = "umd"
            //noStdlib = true
            metaInfo = true
            sourceMap = true
            sourceMapEmbedSources = "always"
        }
    }

    tasks.withType<org.jetbrains.kotlin.gradle.dsl.KotlinJvmCompile> {
        kotlinOptions {
            kotlinOptions {
                jvmTarget = "1.$javaVersion"
                freeCompilerArgs = ktFreeCompilerArgsJvm.split(';').toList()
            }
        }

        doLast {
            copy {
                from ("build/processedResources/jvm/main")
                into ("build/classes/kotlin/jvm/main")
            }
        }
    }
}
