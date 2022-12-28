import io.github.gciatto.kt.mpp.ProjectExtensions.jsProjects
import io.github.gciatto.kt.mpp.ProjectExtensions.ktProjects
import io.github.gciatto.kt.node.NpmPublishExtension

plugins {
    kotlin("multiplatform") version "1.8.0"
    id("org.danilopianini.git-sensitive-semantic-versioning") version "0.3.0"
    id("io.github.gciatto.kt-mpp-pp") version "0.3.5"
}

repositories {
    mavenCentral()
//    maven("https://dl.bintray.com/pika-lab/tuprolog/")
//    jcenter()
}

gitSemVer {
    minimumVersion.set("0.1.0")
    developmentIdentifier.set("dev")
    noTagIdentifier.set("archeo")
    developmentCounterLength.set(2) // How many digits after `dev`
    assignGitSemanticVersion()
}

group = "it.unibo.tuprolog.argumentation"

subprojects {
    group = rootProject.group
    version = rootProject.version
    repositories.addAll(rootProject.repositories)
}

kotlinMultiplatform {
    preventPublishingOfRootProject.set(true)
    developer("Giuseppe Pisano", "g.pisano@unibo.it", "https://www.unibo.it/sitoweb/g.pisano/en")
    jvmOnlyProjects("ide", "actor-solver")
    otherProjects("doc")
    ktProjects(allOtherSubprojects)
}

kotlin {
    sourceSets {
        val commonMain by getting {
            dependencies {
                api(project(":core"))
            }
        }
    }
}

(ktProjects + jsProjects).forEach { project ->
    project.configure<NpmPublishExtension> {
        liftPackageJson {
            dependencies = dependencies?.mapKeys { (key, _) ->
                key.takeIf { it.startsWith("2p-") }?.let { "@tuprolog/$it" } ?: key
            }?.toMutableMap()
        }

        liftJsSources { _, _, line ->
            line.replace("'2p", "'@tuprolog/2p")
                .replace("\"2p", "\"@tuprolog/2p")
        }
    }
}
