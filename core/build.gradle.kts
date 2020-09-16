val tuPrologVersion: String by project

kotlin {
    sourceSets {
        val commonMain by getting {
            dependencies {
                implementation("it.unibo.tuprolog:solve-classic:$tuPrologVersion")
                implementation("it.unibo.tuprolog:parser-theory:$tuPrologVersion")
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

fun File