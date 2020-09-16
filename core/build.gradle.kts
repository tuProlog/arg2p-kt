import org.jetbrains.kotlin.gradle.dsl.KotlinJsCompile
import kotlin.streams.asSequence

val tuPrologVersion: String by project

kotlin {
    sourceSets {
        val commonMain by getting {
            dependencies {
                implementation("it.unibo.tuprolog:solve-classic:$tuPrologVersion")
                implementation("it.unibo.tuprolog:parser-theory:$tuPrologVersion")
                implementation(npm("@tuprolog/parser-utils", "0.2.2"))
                implementation(npm("antlr4", "4.8.0"))
            }
        }
    }
}

fun File.convertIntoKotlinSource(destinationFolder: File, `package`: String) {
    this.bufferedReader().use { r ->
        val lines = r.lines().asSequence()
        val dest = destinationFolder.resolve("${this.nameWithoutExtension.capitalize()}.kt")
        dest.bufferedWriter().use { w ->
            w.write("package $`package`")
            w.newLine()
            w.newLine()
            w.write("internal val $nameWithoutExtension: String = \"\"\"")
            w.newLine()
            for (line in lines) {
                w.write(line)
                w.newLine()
            }
            w.write("\"\"\"")
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
    println(jsMainDir)
    val pckg = "it.unibo.argumentation.arg2p"
    val destDir = jsMainDir.resolve(pckg.replace('.', '/'))
    doLast {
        for (file in plFiles) {
            file.convertIntoKotlinSource(destDir, pckg)
        }
    }
    // TODO set outputs for this task
    tasks.withType<KotlinJsCompile>().forEach {
        it.dependsOn(this)
    }
}