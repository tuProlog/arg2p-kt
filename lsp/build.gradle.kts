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

// The version of what the page of the solver loads (see McpApp): the components of @tuprolog/arg2p-ui, as the extension
// of VS Code builds with them (its package.json, which Renovate keeps up to date)
val pageVersions =
    tasks.register("pageVersions") {
        val packageJson = rootProject.file("vscode/package.json")
        val versions = layout.buildDirectory.file("generated/page/it/unibo/tuprolog/argumentation/lsp/page.properties")
        inputs.file(packageJson)
        outputs.file(versions)
        doLast {
            @Suppress("UNCHECKED_CAST")
            val dependencies = (groovy.json.JsonSlurper().parse(packageJson) as Map<String, Any>)["devDependencies"] as Map<String, String>
            val pinned = Regex("""\d+\.\d+\.\d+(-[\w.]+)?""")
            val name = "@tuprolog/arg2p-ui"
            val ui = dependencies[name]?.takeIf { pinned.matches(it) } ?: error("No exact version of $name in $packageJson")
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
