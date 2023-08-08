import io.github.gciatto.kt.mpp.Plugins
import io.github.gciatto.kt.mpp.ProjectType
import io.github.gciatto.kt.mpp.log
import io.github.gciatto.kt.mpp.nodeVersion

@Suppress("DSL_SCOPE_VIOLATION")
plugins {
    alias(libs.plugins.ktMpp.multiProjectHelper)
    alias(libs.plugins.gitSemVer)
}

group = "it.unibo.tuprolog.argumentation"

allprojects {
    repositories {
        google()
        mavenCentral()
    }
}

gitSemVer {
    excludeLightweightTags()
    assignGitSemanticVersion()
}

multiProjectHelper {
    defaultProjectType = ProjectType.JS

    ktProjects(rootProject.path, ":core", ":arg2p")
    jvmProjects(":ide", ":actor-solver")
    // jsProjects(":js-empty")
    otherProjects(":doc")

    val baseProjectTemplate = buildSet {
        add(Plugins.documentation)
        add(Plugins.versions)
        add(Plugins.linter)
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

project.findProperty("nodeVersion")?.toString()?.takeIf { it.isNotBlank() }?.let {
    nodeVersion(it)
    log("override NodeJS version: $it", LogLevel.LIFECYCLE)
}

afterEvaluate {
    subprojects {
        version = rootProject.version
    }
}

// subprojects {
//    group = rootProject.group
//    version = rootProject.version
//    repositories.addAll(rootProject.repositories)
// }

// kotlinMultiplatform {
//    preventPublishingOfRootProject.set(true)
//    developer("Giuseppe Pisano", "g.pisano@unibo.it", "https://www.unibo.it/sitoweb/g.pisano/en")
//    jvmOnlyProjects("ide", "actor-solver")
//    otherProjects("doc")
//    ktProjects(allOtherSubprojects)
// }

// kotlin {
//    sourceSets {
//        commonMain {
//            dependencies {
//                api(project(":core"))
//            }
//        }
//    }
// }

// (ktProjects + jsProjects).forEach { project ->
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
// }
