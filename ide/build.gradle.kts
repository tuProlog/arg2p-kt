plugins {
    // application
    id(
        libs.plugins.ktMpp.mavenPublish
            .get()
            .pluginId,
    )
    id(
        libs.plugins.ktMpp.fatJar
            .get()
            .pluginId,
    )
}

multiPlatformHelper {
    fatJarEntryPoint.set("it.unibo.tuprolog.argumentation.ui.gui.Main")
}

tasks.shadowJar {
    manifest {
        attributes(
            "Main-Class" to "it.unibo.tuprolog.argumentation.ui.gui.Main",
        )
    }
}

dependencies {
    // JUNG DEPENDENCIES
    api(libs.logback.classic)
    api(libs.logback.core)
    api(libs.jung.api)
    api(libs.jung.visualization)
    api(libs.jung.graphimpl)
    api(libs.jung.algorithms)
    api(libs.jung.io)

    implementation(libs.tuprolog.ide.swing)
    implementation(libs.tuprolog.dsl.solve)
    implementation(libs.tuprolog.solve.classic)

    implementation(project(":core"))
    implementation(project(":actor-solver"))
    implementation(project(":causality-solver"))

    testImplementation(kotlin("test-junit"))
}

tasks.register<JavaExec>("run") {
    group = "application"
    mainClass.set(multiPlatformHelper.fatJarEntryPoint)
    dependsOn("jvmMainClasses")
    sourceSets.getByName("main") {
        classpath = runtimeClasspath
    }
    standardInput = System.`in`
    project.findProperty("arguments")?.let {
        args = it.toString().split("\\s+".toRegex()).filterNot(String::isBlank)
    }
}
