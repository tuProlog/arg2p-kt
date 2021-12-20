val akkaVersion: String by project
val tuPrologVersion: String by project

dependencies {
    api(project(":core"))
    testImplementation(kotlin("test-junit"))

    implementation(platform("com.typesafe.akka:akka-bom_$akkaVersion:2.6.17"))
    implementation("com.typesafe.akka:akka-actor-typed_$akkaVersion")
    implementation("com.typesafe.akka:akka-cluster-sharding-typed_$akkaVersion")
    implementation("com.typesafe.akka:akka-serialization-jackson_$akkaVersion")
    implementation("com.fasterxml.jackson.module:jackson-module-kotlin:2.13.0")

    testImplementation("com.typesafe.akka:akka-actor-testkit-typed_$akkaVersion")
    implementation("ch.qos.logback:logback-classic:1.2.6")

    implementation("it.unibo.tuprolog:solve-classic:$tuPrologVersion")
    implementation("it.unibo.tuprolog:parser-theory:$tuPrologVersion")
}
