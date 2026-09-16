val baseUrl: String? = findProperty("baseUrl")?.toString()

plugins {
    id("io.github.fstaudt.hugo") version "0.13.0"
}

hugo {
    version = "0.166.0"
}

// The latest released version, as advertised by the dependency snippets in the site.
// The project version is not usable here: the documentation is deployed from master, where
// git-sem-ver yields a development version (e.g. 0.16.3-dev02-fc204a3) rather than a released one.
val latestReleasedVersion =
    providers.exec {
        commandLine("git", "describe", "--tags", "--abbrev=0")
    }.standardOutput.asText.map { it.trim() }

val generateVersionData =
    tasks.register("generateVersionData") {
        val version = latestReleasedVersion
        val destination = layout.projectDirectory.file("site/data/arg2p.json")
        inputs.property("version", version)
        outputs.file(destination)
        doLast {
            destination.asFile.apply {
                parentFile.mkdirs()
                writeText("{ \"version\": \"${version.get()}\" }\n")
            }
        }
    }

tasks.hugoBuild {
    dependsOn(generateVersionData)
    args = "--gc --minify --baseURL $baseUrl"
}
