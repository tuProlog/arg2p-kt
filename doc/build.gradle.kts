val orchidVersion: String by project
val orchidBaseUrl: String? by project

plugins {
    id("com.eden.orchidPlugin") version "0.21.1"
}

dependencies {
    // orchidRuntimeOnly("io.github.javaeden.orchid:OrchidDocs:$orchidVersion")
    // orchidRuntimeOnly("io.github.javaeden.orchid:OrchidEditorial:$orchidVersion")
}

@Suppress("Deprecation")
repositories {
    maven("https://jitpack.io")
    jcenter()
    maven("https://kotlin.bintray.com/kotlinx")
}

orchid {
    theme = "Editorial"
    baseUrl = orchidBaseUrl
    version = rootProject.version.toString()
}
