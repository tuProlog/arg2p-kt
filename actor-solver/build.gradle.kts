plugins {
    id(
        libs.plugins.ktMpp.mavenPublish
            .get()
            .pluginId,
    )
}

dependencies {
    // the public API exposes the types of the core (Arg2pSolver.parallel())
    api(project(":core"))
    testImplementation(kotlin("test-junit"))

    implementation(platform(libs.akka.bom))
    implementation(libs.akka.actor.typed)
    implementation(libs.akka.cluster.sharding.typed)
    implementation(libs.akka.serialization.jackson)
    implementation(libs.jackson)
    // the logging of Akka goes through SLF4J: a library leaves its backend to the application, the tests use Logback
    testRuntimeOnly(libs.logback.classic)

    testImplementation(libs.akka.actor.testkit.typed)

    implementation(libs.tuprolog.dsl.solve)
    implementation(libs.tuprolog.parser.theory)
}
