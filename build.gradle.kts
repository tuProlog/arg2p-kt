import io.github.gciatto.kt.mpp.ProjectExtensions.ktProjects

plugins {
    id("io.github.gciatto.kt-mpp-pp") version "0.3.0"
}

repositories {
    maven("https://dl.bintray.com/pika-lab/tuprolog/")
    mavenCentral()
    jcenter()
}

group = "it.unibo.tuprolog.argumentation"

val mochaTimeout: String by project

subprojects {
    group = rootProject.group
    version = rootProject.version
    repositories.addAll(rootProject.repositories)
}

kotlinMultiplatform {
    preventPublishingOfRootProject.set(true)
    developer("Giuseppe Pisano", "g.pisano@unibo.it", "")
    jvmOnlyProjects("ide")
    ktProjects(allOtherSubprojects)
}

//ktProjects.forEach {
//    kotlin {
//        sourceSets {
//            js {
//                nodejs {
//                    testTask {
//                        useMocha {
//                            timeout = mochaTimeout
//                        }
//                    }
//                }
//            }
//        }
//    }
//}
