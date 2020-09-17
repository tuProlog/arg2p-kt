import org.jetbrains.kotlin.gradle.dsl.KotlinJsCompile
import kotlin.streams.asSequence
import java.io.File

val tuPrologVersion: String by project

kotlin {
    sourceSets {
        val commonMain by getting {
            dependencies {
                implementation("it.unibo.tuprolog:solve-classic:$tuPrologVersion")
                implementation("it.unibo.tuprolog:parser-theory:$tuPrologVersion")
            }
        }

        val jsMain by getting {
            dependencies {
                implementation("it.unibo.tuprolog:parser-js:$tuPrologVersion")
                implementation(npm("@tuprolog/parser-utils", "0.2.2"))
                implementation(npm("antlr4", "4.8.0"))
            }
        }
    }
}

private val PL_COMMENT_REGEX = """^\s*%.*""".toRegex()

val String.isSkipable: Boolean get() {
    return isBlank() || PL_COMMENT_REGEX.matches(this)
}

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
            w.write("    val theoryCode: String = \"\"\"")
            w.newLine()
            for (line in lines) {
                if (!line.isSkipable) {
                    w.write("    ")
                    w.write(line)
                    w.newLine()
                }
            }
            w.write("    \"\"\".trimIndent()")
            w.newLine()
            w.write("}")
            w.newLine()
        }
    }
}

tasks.create("generateJsSourcesFromJvmResources", DefaultTask::class) {
    val jvmResourcesDir = kotlin.jvm().compilations["main"].kotlinSourceSets.single().resources.sourceDirectories.single()
    val plFiles = fileTree(jvmResourcesDir).also{
        it.include("**/*.pl")
    }.files
    val jsMainDir = kotlin.js().compilations["main"].kotlinSourceSets.single().kotlin.sourceDirectories.first()
    val pckg = "it.unibo.argumentation.arg2p"
    val destDir = jsMainDir.resolve(pckg.replace('.', '/'))
    for (file in plFiles) {
        inputs.file(file)
        outputs.file(file.resolveDest(destDir))
    }
    tasks.withType<KotlinJsCompile>().forEach {
        it.dependsOn(this)
    }
    doLast {
        for (file in plFiles) {
            file.convertIntoKotlinSource(destDir, pckg)
        }
    }
}