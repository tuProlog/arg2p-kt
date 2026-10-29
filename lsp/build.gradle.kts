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

// The version of what the page of the solver loads (see McpApp): the components of @tuprolog/arg2p-ui, of the catalog
val pageVersions =
    tasks.register("pageVersions") {
        val ui = libs.versions.arg2pUi.get()
        val versions = layout.buildDirectory.file("generated/page/it/unibo/tuprolog/argumentation/lsp/page.properties")
        inputs.property("ui", ui)
        outputs.file(versions)
        doLast {
            versions.get().asFile.writeText("ui=$ui\n")
        }
    }

sourceSets.main {
    resources.srcDir(files(layout.buildDirectory.dir("generated/page")).builtBy(pageVersions))
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
