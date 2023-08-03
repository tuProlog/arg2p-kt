import org.gradle.api.file.DuplicatesStrategy.INCLUDE
import org.jetbrains.kotlin.gradle.dsl.KotlinJsCompile
import java.io.File
import kotlin.streams.asSequence

val tuPrologVersion: String by project

plugins {
    id(libs.plugins.ktMpp.mavenPublish.get().pluginId)
}

kotlin {
    sourceSets {
        commonMain {
            dependencies {
                implementation("it.unibo.tuprolog:dsl-solve:$tuPrologVersion")
                implementation("it.unibo.tuprolog:solve-classic:$tuPrologVersion")
                implementation("it.unibo.tuprolog:parser-theory:$tuPrologVersion")
            }
        }

        commonTest {
            dependencies {
                implementation("it.unibo.tuprolog:test-solve:$tuPrologVersion")
            }
        }
    }
}

private val PL_COMMENT_REGEX =
    """^\s*%.*""".toRegex()

val String.isSkippable: Boolean get() =
    isBlank() || PL_COMMENT_REGEX.matches(this)

fun File.resolveDest(destinationFolder: File): File =
    destinationFolder.resolve("${this.nameWithoutExtension.capitalize()}.kt")

fun File.convertIntoKotlinSource(destinationFolder: File, `package`: String) {
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
    val plFiles = fileTree(jvmResourcesDir).also {
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
