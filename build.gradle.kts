import io.github.gciatto.kt.mpp.Plugins
import io.github.gciatto.kt.mpp.helpers.ProjectType

plugins {
    alias(libs.plugins.ktMpp.helper)
    alias(libs.plugins.ktMpp.mavenPublish)
    alias(libs.plugins.ktMpp.npmPublish)
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

    ktProjects(rootProject.path, ":core", ":causality-solver")
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
                api(project(":core"))
                api(project(":causality-solver"))
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
