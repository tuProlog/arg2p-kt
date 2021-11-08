val akkaVersion: String by project
val tuPrologVersion: String by project

dependencies {
    api(project(":core"))
    testImplementation(kotlin("test-junit"))

    implementation(platform("com.typesafe.akka:akka-bom_$akkaVersion:2.6.17"))
    implementation("com.typesafe.akka:akka-actor-typed_$akkaVersion")
    testImplementation("com.typesafe.akka:akka-actor-testkit-typed_$akkaVersion")
    implementation("ch.qos.logback:logback-classic:1.2.5")

    implementation("it.unibo.tuprolog:solve-classic:$tuPrologVersion")
    implementation("it.unibo.tuprolog:parser-theory:$tuPrologVersion")
}
