import io.github.gciatto.kt.mpp.Plugins
import io.github.gciatto.kt.mpp.helpers.ProjectType
import org.gradle.jvm.toolchain.JavaLanguageVersion
import org.gradle.jvm.toolchain.JavaToolchainService

plugins {
    alias(libs.plugins.ktMpp.helper)
    alias(libs.plugins.ktMpp.mavenPublish)
    alias(libs.plugins.ktMpp.npmPublish)
    alias(libs.plugins.ktMpp.multiplatform)
    alias(libs.plugins.gitSemVer)
}

group = "it.unibo.tuprolog.argumentation"

val jvmVersion: String = libs.versions.jvm.get()

allprojects {
    repositories {
        google()
        mavenCentral()
    }

    // Gradle itself runs on the JDK set in gradle/gradle-daemon-jvm.properties,
    // while artifacts target (and tests run on) the JVM version from the catalog, unless -PtestJvm is provided
    tasks.withType<Test>().configureEach {
        javaLauncher.set(
            project.extensions.getByType<JavaToolchainService>().launcherFor {
                languageVersion.set(JavaLanguageVersion.of(project.findProperty("testJvm")?.toString() ?: jvmVersion))
            },
        )
    }
    tasks.withType<JavaCompile>().configureEach {
        options.release.set(jvmVersion.toInt())
    }
}

gitSemVer {
    buildMetadataSeparator.set("-")
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
