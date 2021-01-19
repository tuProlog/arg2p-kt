import com.github.jengelman.gradle.plugins.shadow.tasks.ShadowJar

val tuPrologVersion: String by project
val javaFxVersion: String by project

plugins {
    application
    id("org.openjfx.javafxplugin") version "0.0.9"
    id("com.github.johnrengelman.shadow") version "6.1.0"
}

dependencies {
    runtimeOnly("org.openjfx:javafx-graphics:$javaFxVersion:win")
    runtimeOnly("org.openjfx:javafx-graphics:$javaFxVersion:linux")
    runtimeOnly("org.openjfx:javafx-graphics:$javaFxVersion:mac")

    implementation("it.unibo.tuprolog:ide:$tuPrologVersion")
    api(project(":core"))
    testImplementation(kotlin("test-junit"))
}

javafx {
    version = javaFxVersion
    modules = listOf("javafx.controls", "javafx.fxml", "javafx.graphics")
}

val entryPoint = "it.unibo.tuprolog.argumentation.ui.gui.Main"

application {
    mainClassName = entryPoint
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
