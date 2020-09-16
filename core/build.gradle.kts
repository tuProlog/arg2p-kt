kotlin {
    sourceSets {
        val commonMain by getting {
            dependencies {
                implementation("it.unibo.tuprolog:solve-classic:0.11.5-dev10+28f0f968")
                implementation("it.unibo.tuprolog:parser-theory:0.11.5-dev10+28f0f968")
            }
        }
    }
}

tasks.withType<org.jetbrains.kotlin.gradle.dsl.KotlinJsCompile> {
    doLast {
        copy {
            from ("build/processedResources/js/main")
            into ("../build/js/packages/arg2p-kt-core/kotlin")
        }
    }
}