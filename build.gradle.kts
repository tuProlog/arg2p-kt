import io.github.gciatto.kt.mpp.Plugins
import io.github.gciatto.kt.mpp.helpers.ProjectType
// import io.github.gciatto.kt.mpp.nodeVersion

@Suppress("DSL_SCOPE_VIOLATION")
plugins {
    alias(libs.plugins.ktMpp.helper)
    alias(libs.plugins.ktMpp.mavenPublish)
    alias(libs.plugins.ktMpp.multiplatform)
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

    ktProjects(rootProject.path, ":core")
    jvmProjects(":ide", ":actor-solver")
    // jsProjects()
    otherProjects(":doc")

    val baseProjectTemplate =
        buildSet {
            add(Plugins.documentation)
            add(Plugins.versions)
            add(Plugins.linter)
        }

    ktProjectTemplate =
        buildSet {
            addAll(baseProjectTemplate)
            add(Plugins.multiplatform)
        }

    jvmProjectTemplate =
        buildSet {
            addAll(baseProjectTemplate)
            add(Plugins.jvmOnly)
        }

    jsProjectTemplate =
        buildSet {
            addAll(baseProjectTemplate)
            add(Plugins.jsOnly)
        }

    otherProjectTemplate =
        buildSet {
            add(Plugins.versions)
        }

    applyProjectTemplates()
}

kotlin {
    sourceSets {
        commonMain {
            dependencies {
                api(project(":core"))
            }
        }
    }
}

// project.findProperty("nodeVersion")?.toString()?.takeIf { it.isNotBlank() }?.let {
//    nodeVersion(it)
//    log("override NodeJS version: $it", LogLevel.LIFECYCLE)
// }

afterEvaluate {
    subprojects {
        version = rootProject.version
    }
}
