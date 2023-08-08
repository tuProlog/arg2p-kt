plugins {
    id(libs.plugins.ktMpp.mavenPublish.get().pluginId)
    id(libs.plugins.ktMpp.npmPublish.get().pluginId)
}

kotlin {
    sourceSets {
        commonMain {
            dependencies {
                api(project(":core"))
            }
        }
    }
}
