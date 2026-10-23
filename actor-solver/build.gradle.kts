plugins {
    id(
        libs.plugins.ktMpp.mavenPublish
            .get()
            .pluginId,
    )
}

dependencies {
    // the public API exposes the types of the core (Arg2pSolver.parallel(), ActorSolver)
    api(project(":core"))
    testImplementation(kotlin("test-junit"))

    implementation(platform(libs.akka.bom))
    implementation(libs.akka.actor.typed)
    implementation(libs.akka.cluster.sharding.typed)
    implementation(libs.akka.serialization.jackson)
    implementation(libs.jackson)
    implementation(libs.logback.classic)

    testImplementation(libs.akka.actor.testkit.typed)

    implementation(libs.tuprolog.dsl.solve)
    implementation(libs.tuprolog.parser.theory)
}
