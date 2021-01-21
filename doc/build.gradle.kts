val orchidVersion: String by project
val orchidBaseUrl: String? by project

plugins {
    id("com.eden.orchidPlugin") version "0.21.1"
}

dependencies {
    orchidRuntimeOnly("io.github.javaeden.orchid:OrchidDocs:$orchidVersion")
    orchidRuntimeOnly("io.github.javaeden.orchid:OrchidEditorial:$orchidVersion")
}

repositories {
    mavenCentral()
    jcenter()
}

orchid {
    theme = "Editorial"
    baseUrl = orchidBaseUrl
    version = rootProject.version.toString()
}
