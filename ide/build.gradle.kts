import com.github.jengelman.gradle.plugins.shadow.tasks.ShadowJar

plugins {
    application
    id(libs.plugins.ktMpp.mavenPublish.get().pluginId)
    alias(libs.plugins.shadowJar)
    alias(libs.plugins.javaFx)
}

val supportedPlatforms by extra { listOf("win", "linux", "mac") }

dependencies {

    for (platform in supportedPlatforms) {
        val dependency = libs.javaFxLib.get().let {
            "${it.module.group}:${it.module.name}:${it.versionConstraint.requiredVersion}"
        }
        runtimeOnly("$dependency:$platform")
    }

    /* JUNG DEPENDENCIES */
    api(libs.logback.classic)
    api(libs.logback.core)
    api(libs.jung.api)
    api(libs.jung.visualization)
    api(libs.jung.graphimpl)
    api(libs.jung.algorithms)
    api(libs.jung.io)

    implementation(libs.tuprolog.ide)
    implementation(libs.tuprolog.dsl.solve)
    implementation(libs.tuprolog.solve.classic)

    implementation(project(":core"))
    implementation(project(":actor-solver"))

    testImplementation(kotlin("test-junit"))
}

javafx {
    version = libs.javaFxLib.get().version
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
