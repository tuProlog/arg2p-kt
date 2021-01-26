plugins {
    id("org.danilopianini.git-sensitive-semantic-versioning") version "0.2.3"
    id("io.github.gciatto.kt-mpp-pp") version "0.3.0"
}

repositories {
    mavenCentral()
    maven("https://dl.bintray.com/pika-lab/tuprolog/")
    jcenter()
}

gitSemVer {
    minimumVersion.set("0.1.0")
    developmentIdentifier.set("dev")
    noTagIdentifier.set("archeo")
    developmentCounterLength.set(2) // How many digits after `dev`
    version = computeGitSemVer() // THIS IS MANDATORY, AND MUST BE LAST IN THIS BLOCK!
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
    otherProjects("doc")
    ktProjects(allOtherSubprojects)
}
