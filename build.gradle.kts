plugins {
    id("io.github.gciatto.kt-mpp-pp") version "0.3.0"
}

repositories {
    mavenCentral()
    maven("https://dl.bintray.com/pika-lab/tuprolog/")
    jcenter()
}

group = "it.unibo.tuprolog.argumentation"

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
