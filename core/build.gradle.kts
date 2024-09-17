import org.gradle.api.file.DuplicatesStrategy.INCLUDE
import org.jetbrains.kotlin.gradle.targets.jvm.tasks.KotlinJvmTest
import java.io.File
import kotlin.streams.asSequence

val jvmStackSize: String by project
val jvmMaxHeapSize: String by project

plugins {
    id(libs.plugins.ktMpp.mavenPublish.get().pluginId)
    id(libs.plugins.ktMpp.npmPublish.get().pluginId)
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
                implementation(libs.tuprolog.dsl.solve)
                implementation(libs.tuprolog.solve.classic)
                implementation(libs.tuprolog.parser.theory)
            }
        }

        commonTest {
            dependencies {
                implementation(libs.tuprolog.test.solve)
            }
        }

        val jsMain by getting
    }
}

tasks.withType<KotlinJvmTest> {
    maxHeapSize = jvmMaxHeapSize
    jvmArgs("-Xss$jvmStackSize")
}

val String.isSkippable: Boolean get() =
    isBlank() || """^\s*%.*""".toRegex().matches(this)

fun File.resolveDest(destinationFolder: File): File = destinationFolder.resolve("${this.nameWithoutExtension.capitalize()}.kt")

fun File.convertIntoKotlinSource(
    destinationFolder: File,
    `package`: String,
) {
    this.bufferedReader().use { r ->
        val lines = r.lines().asSequence()
        val dest = resolveDest(destinationFolder)
        dest.bufferedWriter().use { w ->
            w.write("package $`package`")
            w.newLine()
            w.newLine()
            w.write("object ${nameWithoutExtension.capitalize()} {")
            w.newLine()
            w.write("    val theoryCode: String =")
            w.newLine()
            w.write("        \"\"\"")
            w.newLine()
            for (line in lines) {
                if (!line.isSkippable) {
                    w.write("    ")
                    w.write(line)
                    w.newLine()
                }
            }
            w.write("        \"\"\".trimIndent()")
            w.newLine()
            w.write("}")
            w.newLine()
        }
    }
}

tasks.create("generateJsSourcesFromJvmResources", DefaultTask::class) {
    val jvmResourcesDir = kotlin.jvm().compilations["main"].kotlinSourceSets.single().resources.sourceDirectories.single()
    val jsMainDir = kotlin.js().compilations["main"].kotlinSourceSets.single().kotlin.sourceDirectories.first()
    val plFiles =
        fileTree(jvmResourcesDir).also {
            it.include("**/*.pl")
        }.files
    val packageName = "it.unibo.tuprolog.argumentation.core.libs.sources"
    val destDir = jsMainDir.resolve(packageName.replace('.', '/'))

    for (file in plFiles) {
        inputs.file(file)
        outputs.file(file.resolveDest(destDir))
    }

    tasks.compileKotlinJs.get().dependsOn(this)
    tasks.sourcesJar.get().dependsOn(this)
    tasks.jsSourcesJar.get().dependsOn(this)
    tasks.dokkaHtml.get().dependsOn(this)

    doLast {
        for (file in plFiles) {
            file.convertIntoKotlinSource(destDir, packageName)
        }
    }
}

tasks.getByName<Copy>("jvmProcessResources") {
    duplicatesStrategy = INCLUDE
}
