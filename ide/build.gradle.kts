import com.github.jengelman.gradle.plugins.shadow.tasks.ShadowJar
// import com.github.breadmoirai.githubreleaseplugin.GithubReleaseExtension
// import com.github.breadmoirai.githubreleaseplugin.GithubReleaseTask
// import io.github.gciatto.kt.mpp.ProjectConfiguration.configureUploadToGithub

val tuPrologVersion: String by project
val javaFxVersion: String by project

plugins {
    application
    id("org.openjfx.javafxplugin") version "0.0.14"
    id("com.github.johnrengelman.shadow") version "8.1.1"
    // id(libs.plugins.shadowJar.get())
}

dependencies {
    runtimeOnly("org.openjfx:javafx-graphics:$javaFxVersion:win")
    runtimeOnly("org.openjfx:javafx-graphics:$javaFxVersion:linux")
    runtimeOnly("org.openjfx:javafx-graphics:$javaFxVersion:mac")

    /* JUNG DEPENDENCIES */
    api("ch.qos.logback", "logback-classic", "1.4.8")
    api("ch.qos.logback", "logback-core", "1.4.8")
    api("net.sf.jung", "jung-api", "2.1.1")
    api("net.sf.jung", "jung-visualization", "2.1.1")
    api("net.sf.jung", "jung-graph-impl", "2.1.1")
    api("net.sf.jung", "jung-algorithms", "2.1.1")
    api("net.sf.jung", "jung-io", "2.1.1")

    implementation("it.unibo.tuprolog:ide:$tuPrologVersion")
    implementation("it.unibo.tuprolog:dsl-solve:$tuPrologVersion")
    implementation("it.unibo.tuprolog:solve-classic:$tuPrologVersion")
    implementation(project(":core"))
    implementation(project(":actor-solver"))

    testImplementation(kotlin("test-junit"))

//    val entryPoint = "it.unibo.tuprolog.argumentation.ui.gui.Main"
//
//    tasks.create<JavaExec>("run") {
//        group = "application"
//        mainClass.set(entryPoint)
//        dependsOn("jvmMainClasses")
//        sourceSets.getByName("main") {
//            classpath = runtimeClasspath
//        }
//        standardInput = System.`in`
//    }
//
//    shadowJar(entryPoint)
}

javafx {
    version = javaFxVersion
    modules = listOf("javafx.controls", "javafx.fxml", "javafx.graphics", "javafx.swing")
}

val entryPoint = "it.unibo.tuprolog.argumentation.ui.gui.Main"

application {
    mainClass = entryPoint
}

val shadowJar = tasks.getByName<ShadowJar>("shadowJar") {
    manifest { attributes("Main-Class" to entryPoint) }
    archiveBaseName.set("${rootProject.name}-${project.name}")
    archiveVersion.set(project.version.toString())
    archiveClassifier.set("redist")
    sourceSets.main {
        runtimeClasspath.filter { it.exists() }
            .map { if (it.isDirectory) it else zipTree(it) }
            .forEach {
                from(it)
            }
    }
    dependsOn("classes")
}

tasks.withType<ShadowJar> {
    val newTransformer = com.github.jengelman.gradle.plugins.shadow.transformers.AppendingTransformer()
    newTransformer.resource = "reference.conf"
    transformers.add(newTransformer)
}

// configureUploadToGithub(shadowJar)

// fun Project.configureUploadToGithub(vararg tasks: Zip) {
//    if (ktMpp.githubToken.isPresent.not() || ktMpp.githubToken.get().isBlank()) return
//
//    val archiveFiles = tasks.map { it.archiveFile }
//
//    rootProject.let {
//        it.configure<GithubReleaseExtension> {
//            releaseAssets(*(releaseAssets.toList() + archiveFiles).toTypedArray())
//        }
//
//        it.tasks.withType(GithubReleaseTask::class.java) { releaseTask ->
//            releaseTask.dependsOn(*tasks)
//        }
//    }
// }
//
// fun Project.configureUploadToGithub(
//    jarTaskPositiveFilter: (String) -> Boolean = { "jar" in it },
//    jarTaskNegativeFilter: (String) -> Boolean = { "dokka" in it || "source" in it }
// ) {
//    tasks.withType(Zip::class.java).matching {
//        val name = it.name.toLowerCase()
//        jarTaskPositiveFilter(name) && !jarTaskNegativeFilter(name)
//    }.all {
//        configureUploadToGithub(it)
//    }
// }
