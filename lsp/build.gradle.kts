// The language server of arg2p: the diagnostics of theories, and the solver the editors run them on (see the
// extension of VS Code, in vscode/). A jar with every dependency in it (shadowJar), run as `java -jar`.
plugins {
    id(
        libs.plugins.ktMpp.fatJar
            .get()
            .pluginId,
    )
}

multiPlatformHelper {
    fatJarEntryPoint.set("it.unibo.tuprolog.argumentation.lsp.MainKt")
}

tasks.shadowJar {
    manifest {
        attributes("Main-Class" to "it.unibo.tuprolog.argumentation.lsp.MainKt")
    }
}

val jvmStackSize: String = property("jvmStackSize").toString()

tasks.withType<Test>().configureEach {
    jvmArgs("-Xss$jvmStackSize")
}

dependencies {
    implementation(project(":"))
    implementation(libs.lsp4j)

    testImplementation(kotlin("test-junit"))
}
