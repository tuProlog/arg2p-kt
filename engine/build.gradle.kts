// The machine solving the theories of arg2p, on the interfaces of 2P-Kt: it knows nothing of arg2p, whose core depends on
// it. What the other modules share of it is marked as InternalArg2pApi, the rest is internal
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
                api(libs.tuprolog.dsl.solve)
                api(libs.tuprolog.parser.theory)
            }
        }

        commonTest {
            dependencies {
                implementation(libs.tuprolog.test.solve)
            }
        }
    }
}

// No null checks of the parameters, receivers and platform calls in the machine's code: Kotlin writes one at the entry of
// every non-private function (6 bytes of bytecode each, 26 in Machine alone), and the JIT of the JVM decides what to inline
// by bytecode size (35 bytes always, 325 when hot), so they keep small helpers as alloc from being inlined
tasks.named<org.jetbrains.kotlin.gradle.tasks.KotlinJvmCompile>("compileKotlinJvm") {
    compilerOptions.freeCompilerArgs.addAll("-Xno-param-assertions", "-Xno-call-assertions", "-Xno-receiver-assertions")
}

// What a change to the machine must not break, in seconds (see jvmQuickTest in core)
tasks.register<Test>("jvmQuickTest") {
    group = "verification"
    description = "Runs the fast tests of the machine"
    val jvmTest = tasks.named<KotlinJvmTest>("jvmTest").get()
    dependsOn("jvmTestClasses")
    testClassesDirs = jvmTest.testClassesDirs
    classpath = jvmTest.classpath
    filter.excludeTestsMatching("*.MachineTest.inferenceRate")
    filter.excludeTestsMatching("*.StdlibNativesTest")
    outputs.upToDateWhen { false }
}
