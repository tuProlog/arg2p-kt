plugins {
    id(
        libs.plugins.ktMpp.mavenPublish
            .get()
            .pluginId,
    )
}

dependencies {
    implementation(project(":core"))
    testImplementation(kotlin("test-junit"))

    implementation(libs.akka.bom)
    implementation(libs.akka.actor.typed)
    implementation(libs.akka.cluster.sharding.typed)
    implementation(libs.akka.serialization.jackson)
    implementation(libs.jackson)
    implementation(libs.logback.classic)

    testImplementation(libs.akka.actor.testkit.typed)

    implementation(libs.tuprolog.dsl.solve)
    implementation(libs.tuprolog.solve.classic)
    implementation(libs.tuprolog.parser.theory)
}
