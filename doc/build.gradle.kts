val baseUrl: String? by project

plugins {
    id("io.github.fstaudt.hugo") version "0.10.0"
}

hugo {
    version = "0.150.0"
}

tasks.hugoBuild {
    args = "--baseURL \"$baseUrl/\""
}