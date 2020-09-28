import node.NpmPublishExtension
import node.NpmPublishPlugin
import node.People
import node.Bugs
import org.jetbrains.kotlin.gradle.dsl.KotlinJsCompile
import org.jetbrains.kotlin.gradle.dsl.KotlinJvmCompile
import org.jetbrains.kotlin.gradle.targets.js.nodejs.NodeJsSetupTask
import org.jetbrains.kotlin.gradle.targets.js.npm.tasks.KotlinPackageJsonTask
import org.jetbrains.kotlin.gradle.tasks.Kotlin2JsCompile


plugins {
    kotlin("multiplatform") version Versions.org_jetbrains_kotlin_multiplatform_gradle_plugin apply true
    id("org.danilopianini.git-sensitive-semantic-versioning") version Versions.org_danilopianini_git_sensitive_semantic_versioning_gradle_plugin
    id("de.fayard.buildSrcVersions") version Versions.de_fayard_buildsrcversions_gradle_plugin
}

repositories {
    mavenCentral()
    jcenter()
}

gitSemVer {
    minimumVersion.set("0.1.0")
    developmentIdentifier.set("dev")
    noTagIdentifier.set("archeo")
    developmentCounterLength.set(2) // How many digits after `dev`
    version = computeGitSemVer() // THIS IS MANDATORY, AND MUST BE LAST IN THIS BLOCK!
}

val javaVersion: String by project
val ktFreeCompilerArgsJvm: String by project
val mochaTimeout: String by project
val gpName: String by project
val gpEmail: String by project
val gpUrl: String by project
val projectHomepage: String by project
val projectIssues: String by project

val npmToken = getPropertyOrWarnForAbsence("npmToken")

group = "it.unibo.tuprolog.argumentation"

allprojects {

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
                nodejs {
                    testTask {
                        useMocha {
                            timeout = mochaTimeout
                        }
                    }
                }

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

    tasks.withType<KotlinJsCompile> {
        kotlinOptions {
            moduleKind = "umd"
            metaInfo = true
            sourceMap = true
            sourceMapEmbedSources = "always"
        }
    }

    tasks.withType<KotlinJvmCompile> {
        kotlinOptions {
            kotlinOptions {
                jvmTarget = "1.$javaVersion"
                freeCompilerArgs = ktFreeCompilerArgsJvm.split(';').toList()
            }
        }
    }

    configureJsPackage()
}

fun Project.configureJsPackage(packageJsonTask: String = "jsPackageJson", compileTask: String = "jsMainClasses") {
    if (this == rootProject) return

    apply<NpmPublishPlugin>()

    configure<NpmPublishExtension> {
        nodeRoot = rootProject.tasks.withType<NodeJsSetupTask>().asSequence().map { it.destination }.first()
        token = npmToken ?: ""
        packageJson = tasks.getByName<KotlinPackageJsonTask>(packageJsonTask).packageJson
        nodeSetupTask = rootProject.tasks.getByName("kotlinNodeJsSetup").path
        jsCompileTask = compileTask
        jsSourcesDir = tasks.withType<Kotlin2JsCompile>().asSequence()
            .filter { "Test" !in it.name }
            .map { it.outputFile.parentFile }
            .first()

        liftPackageJson {
            people = mutableListOf(People(gpName, gpEmail))
            homepage = projectHomepage
            bugs = Bugs(projectIssues, gpEmail)
//            license = projectLicense
            name = "@tuprolog/$name"
            dependencies = dependencies?.filterKeys { key -> "kotlin-test" !in key }
                ?.mapKeys { (key, _) ->
                    if ("2p" in key) "@tuprolog/$key" else key
                }?.mapValues { (key, value) ->
                    val temp = if (value.startsWith("file:")) {
                        value.split('/', '\\').last()
                    } else {
                        value
                    }
                    if ("2p" in key) temp.substringBefore('+') else temp
                }?.toMutableMap()
            version = version?.substringBefore('+')
        }

        liftJsSources { _, _, line ->
            line.replace("'2p", "'@tuprolog/2p")
                .replace("\"2p", "\"@tuprolog/2p")
        }
    }
}

