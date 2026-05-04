val baseUrl: String? by project

plugins {
    id("io.github.fstaudt.hugo") version "0.12.0"
}

hugo {
    version = "0.161.1"
}

tasks.hugoBuild {
    args = "--gc --minify --baseURL $baseUrl"
}
