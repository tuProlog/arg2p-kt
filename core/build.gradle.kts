import org.gradle.api.attributes.Attribute
import org.jetbrains.kotlin.gradle.targets.jvm.tasks.KotlinJvmTest

plugins {
    id(
        libs.plugins.ktMpp.mavenPublish
            .get()
            .pluginId,
    )
}

kotlin {
    sourceSets {
        commonMain {
            dependencies {
                api(project(":engine"))
                api(libs.tuprolog.dsl.solve)
                api(libs.tuprolog.parser.theory)
                // The lexer of 2P-Kt, keeping the comments of a theory (see RuleAnnotations)
                implementation(libs.tuprolog.parser.impl)
                // Provides consult/1, used by module::callModule/2 to load external theories.
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

        jvmTest {
            dependencies {
                // the launcher the Prolog coverage runs the tests with (at run time, the one kotlin-test brings)
                compileOnly(libs.junit.platform.launcher)
            }
        }
    }
}

// Timing benchmarks live with the JVM tests but only run through jvmBenchmark (-Pbenchmark.* options)
val benchmarkTests = "it.unibo.tuprolog.argumentation.core.benchmark.*"

// The Prolog coverage of the tests only runs through jvmPrologCoverage, which runs the tests itself: those of core, of
// the causality solver and of the API (the behaviour of arg2p), whose test classes it adds
val coverageTests = "it.unibo.tuprolog.argumentation.core.coverage.*"

tasks.named<KotlinJvmTest>("jvmTest") {
    filter.excludeTestsMatching(benchmarkTests)
    filter.excludeTestsMatching(coverageTests)
}

// The projects whose tests the Prolog coverage runs besides those of core
val coverageProjects = listOf(":causality-solver", ":")

// Their classes and what they need at run time, resolved here, by this project (as a JVM runtime classpath, the
// attributes of the test runtime classpath of core): a configuration of theirs resolved from this project, as their
// test tasks' classpath, is resolved without their lock, which Gradle refuses while they run a task at the same time
val coverageRuntime: Configuration =
    configurations.create("coverageRuntime") {
        isCanBeConsumed = false
        isCanBeResolved = true
        val runtime = configurations.getByName("jvmTestRuntimeClasspath").attributes
        attributes {
            runtime.keySet().forEach {
                @Suppress("UNCHECKED_CAST")
                val key = it as Attribute<Any>
                attribute(key, runtime.getAttribute(key)!!)
            }
        }
    }

dependencies {
    coverageProjects.forEach { coverageRuntime(project(it)) }
}

tasks.register<Test>("jvmPrologCoverage") {
    group = "verification"
    description = "Reports the clauses of the Prolog modules the tests never use (build/reports/prolog-coverage)"
    val jvmTest = tasks.named<KotlinJvmTest>("jvmTest").get()
    dependsOn(listOf("jvmTestClasses") + coverageProjects.map { "${it.removeSuffix(":")}:jvmTestClasses" })
    testClassesDirs = jvmTest.testClassesDirs
    // the test classes of the other projects: the outputs of their compilations, no configuration of theirs resolved
    val otherTestClasses =
        files(
            provider {
                coverageProjects.map {
                    project(it)
                        .tasks
                        .named<KotlinJvmTest>("jvmTest")
                        .get()
                        .testClassesDirs
                }
            },
        )
    classpath = jvmTest.classpath + otherTestClasses + coverageRuntime
    filter.includeTestsMatching(coverageTests)
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

// What a change to the machine must not break, in seconds: the engine, the labels, the modules. Not a substitute for the
// JVM tests of every project before a commit
tasks.register<Test>("jvmQuickTest") {
    group = "verification"
    description = "Runs the fast tests of the engine and of the labels"
    val jvmTest = tasks.named<KotlinJvmTest>("jvmTest").get()
    dependsOn("jvmTestClasses", ":engine:jvmQuickTest")
    testClassesDirs = jvmTest.testClassesDirs
    classpath = jvmTest.classpath
    filter.excludeTestsMatching(benchmarkTests)
    filter.excludeTestsMatching(coverageTests)
    listOf(
        "*.AbaPrefLibraryTest",
        "*.SharedProgramsTest",
        "*.StandardEngineTest",
        "*.SemanticsReferenceTest",
        "*.InlinedCallLibrariesTest",
        "*.ConcurrentSolversTest",
    ).forEach { filter.excludeTestsMatching(it) }
    outputs.upToDateWhen { false }
}

tasks.register<Test>("jvmBenchmark") {
    group = "verification"
    description = "Times the argument labelling semantics on growing abstract frameworks"
    val jvmTest = tasks.named<KotlinJvmTest>("jvmTest").get()
    dependsOn("jvmTestClasses")
    testClassesDirs = jvmTest.testClassesDirs
    classpath = jvmTest.classpath
    filter.includeTestsMatching(benchmarkTests)
    systemProperties(project.properties.filterKeys { it.startsWith("benchmark.") })
    testLogging.showStandardStreams = true
    outputs.upToDateWhen { false }
}
