import org.gradle.kotlin.dsl.assign
import org.gradle.kotlin.dsl.withType
import org.jetbrains.kotlin.gradle.targets.jvm.tasks.KotlinJvmTest

val jvmStackSize: String = property("jvmStackSize").toString()
val jvmMaxHeapSize: String = property("jvmMaxHeapSize").toString()

plugins {
    id(
        libs.plugins.ktMpp.mavenPublish
            .get()
            .pluginId,
    )
    id(
        libs.plugins.ktMpp.npmPublish
            .get()
            .pluginId,
    )
}

kotlin {
    js {
        compilations.all {
            compileTaskProvider.configure {
                compilerOptions.freeCompilerArgs
                    .add("-Xir-minimized-member-names=false")
            }
        }
        binaries.library()
    }

    sourceSets {
        commonMain {
            dependencies {
                api(libs.tuprolog.dsl.solve)
                api(libs.tuprolog.parser.theory)
                // Provides consult/1, used by module::call_module/2 to load external theories.
                implementation(libs.tuprolog.io.lib)
            }
        }

        commonTest {
            dependencies {
                implementation(libs.tuprolog.test.solve)
                // tuProlog's own solver, for the tests of the libraries of arg2p on an engine other than its own
                implementation(libs.tuprolog.solve.prolog)
            }
        }
    }
}

tasks.withType<KotlinJvmTest> {
    maxHeapSize = jvmMaxHeapSize
    jvmArgs("-Xss$jvmStackSize")
}

// Timing benchmarks live with the JVM tests but only run through jvmBenchmark (-Pbenchmark.* options)
val benchmarkTests = "it.unibo.tuprolog.argumentation.core.benchmark.*"

// The Prolog coverage of the tests only runs through jvmPrologCoverage, which runs the tests itself
val coverageTests = "it.unibo.tuprolog.argumentation.core.coverage.*"

tasks.named<KotlinJvmTest>("jvmTest") {
    filter.excludeTestsMatching(benchmarkTests)
    filter.excludeTestsMatching(coverageTests)
}

tasks.register<Test>("jvmPrologCoverage") {
    group = "verification"
    description = "Reports the clauses of the Prolog modules the tests never use (build/reports/prolog-coverage)"
    val jvmTest = tasks.named<KotlinJvmTest>("jvmTest").get()
    dependsOn("jvmTestClasses")
    testClassesDirs = jvmTest.testClassesDirs
    classpath = jvmTest.classpath
    filter.includeTestsMatching(coverageTests)
    maxHeapSize = jvmMaxHeapSize
    jvmArgs("-Xss$jvmStackSize")
    testLogging.showStandardStreams = true
    outputs.upToDateWhen { false }
}

tasks.register<JavaExec>("prologDocs") {
    group = "documentation"
    description = "Writes the pages of the Prolog modules for the documentation site, from their sources (build/docs/prolog-modules)"
    dependsOn("jvmTestClasses")
    val jvmTest = tasks.named<KotlinJvmTest>("jvmTest").get()
    classpath = jvmTest.classpath
    mainClass = "it.unibo.tuprolog.argumentation.core.docs.PrologDocsKt"
    args(
        file("src/commonMain/kotlin").absolutePath,
        layout.buildDirectory
            .dir("docs/prolog-modules")
            .get()
            .asFile.absolutePath,
    )
}

tasks.register<Test>("jvmBenchmark") {
    group = "verification"
    description = "Times the argument labelling semantics on growing abstract frameworks"
    val jvmTest = tasks.named<KotlinJvmTest>("jvmTest").get()
    dependsOn("jvmTestClasses")
    testClassesDirs = jvmTest.testClassesDirs
    classpath = jvmTest.classpath
    filter.includeTestsMatching(benchmarkTests)
    maxHeapSize = jvmMaxHeapSize
    jvmArgs("-Xss$jvmStackSize")
    systemProperties(project.properties.filterKeys { it.startsWith("benchmark.") })
    testLogging.showStandardStreams = true
    outputs.upToDateWhen { false }
}

// The description of the module in the Kotlin & JS reference of the documentation site
extensions.configure<org.jetbrains.dokka.gradle.DokkaExtension> {
    dokkaSourceSets.configureEach { includes.from("Module.md") }
}
