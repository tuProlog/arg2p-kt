import io.github.gciatto.kt.mpp.Plugins
import io.github.gciatto.kt.mpp.ProjectType

@Suppress("DSL_SCOPE_VIOLATION")
plugins {
    alias(libs.plugins.ktMppHelper)
    alias(libs.plugins.gitSemVer)
    // id(libs.plugins.ktMpp.mavenPublish.get().pluginId)
}

group = "it.unibo.tuprolog.argumentation"

allprojects {
    repositories {
        mavenCentral()
    }
}

gitSemVer {
    minimumVersion.set("0.1.0")
    developmentIdentifier.set("dev")
    noTagIdentifier.set("archeo")
    developmentCounterLength.set(2)
    assignGitSemanticVersion()
}


multiProjectHelper {
    defaultProjectType = ProjectType.JS

    ktProjects(rootProject.path, ":core")
    jvmProjects(":ide", ":actor-solver")
    // jsProjects(":js-empty")
    otherProjects(":doc")

    val baseProjectTemplate = buildSet {
        add(Plugins.documentation)
        add(Plugins.versions)
    }

    ktProjectTemplate = buildSet {
        addAll(baseProjectTemplate)
        add(Plugins.multiplatform)
    }

    jvmProjectTemplate = buildSet {
        addAll(baseProjectTemplate)
        add(Plugins.jvmOnly)
    }

    jsProjectTemplate = buildSet {
        addAll(baseProjectTemplate)
        add(Plugins.jsOnly)
    }

    otherProjectTemplate = buildSet {
        add(Plugins.versions)
    }

    applyProjectTemplates()
}


//subprojects {
//    group = rootProject.group
//    version = rootProject.version
//    repositories.addAll(rootProject.repositories)
//}

//kotlinMultiplatform {
//    preventPublishingOfRootProject.set(true)
//    developer("Giuseppe Pisano", "g.pisano@unibo.it", "https://www.unibo.it/sitoweb/g.pisano/en")
//    jvmOnlyProjects("ide", "actor-solver")
//    otherProjects("doc")
//    ktProjects(allOtherSubprojects)
//}

//kotlin {
//    sourceSets {
//        commonMain {
//            dependencies {
//                api(project(":core"))
//            }
//        }
//    }
//}

//(ktProjects + jsProjects).forEach { project ->
//    project.configure<NpmPublishExtension> {
//        liftPackageJson {
//            dependencies = dependencies?.mapKeys { (key, _) ->
//                key.takeIf { it.startsWith("2p-") }?.let { "@tuprolog/$it" } ?: key
//            }?.toMutableMap()
//        }
//
//        liftJsSources { _, _, line ->
//            line.replace("'2p", "'@tuprolog/2p")
//                .replace("\"2p", "\"@tuprolog/2p")
//        }
//    }
//}
