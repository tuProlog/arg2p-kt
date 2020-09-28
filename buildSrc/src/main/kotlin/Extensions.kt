import org.gradle.api.Project
import org.gradle.api.publish.maven.MavenPublication
import org.gradle.api.tasks.testing.AbstractTestTask
import org.gradle.api.tasks.testing.TestDescriptor
import org.gradle.api.tasks.testing.TestResult
import org.gradle.kotlin.dsl.KotlinClosure2
import org.gradle.kotlin.dsl.withType

private val FULL_VERSION_REGEX = "^[0-9]+\\.[0-9]+\\.[0-9]+$".toRegex()

val Project.isFullVersion: Boolean
    get() = version.toString().matches(FULL_VERSION_REGEX)

fun Project.configureTestResultPrinting() {
    tasks.withType<AbstractTestTask> {
        afterSuite(KotlinClosure2({ desc: TestDescriptor, result: TestResult ->
            if (desc.parent == null) { // will match the outermost suite
                println("Results: ${result.resultType} (${result.testCount} tests, ${result.successfulTestCount} successes, ${result.failedTestCount} failures, ${result.skippedTestCount} skipped)")
            }
        }))
    }
}

fun MavenPublication.configurePom(projectName: String) {
    //TODO add pom file
}

fun log(message: String) {
    System.out.println("LOG: $message")
}

fun warn(message: String) {
    System.err.println("WARNING: $message")
}

fun Project.getPropertyOrWarnForAbsence(key: String): String? {
    val value = property(key)?.toString()
    if (value.isNullOrBlank()) {
        warn("$key is not set")
    }
    return value
}

//fun capitalize(s: String) = s[0].toUpperCase() + s.substring(1)

val Project.docDir: String
    get() = "$buildDir/doc"