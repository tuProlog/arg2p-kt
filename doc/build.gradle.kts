val baseUrl: String? = findProperty("baseUrl")?.toString()

plugins {
    id("io.github.fstaudt.hugo") version "0.13.0"
}

hugo {
    version = "0.166.0"
}

tasks.hugoBuild {
    args = "--gc --minify --baseURL $baseUrl"
}
