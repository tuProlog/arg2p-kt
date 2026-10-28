package it.unibo.tuprolog.argumentation

/**
 * A declaration the modules of arg2p share with one another, not part of its API: it may change in any version. The
 * modules of arg2p opt in (see ktCompilerArgs in gradle.properties); its users get an error.
 */
@RequiresOptIn(level = RequiresOptIn.Level.ERROR, message = "Internal to arg2p: it may change in any version")
@Retention(AnnotationRetention.BINARY)
@Target(AnnotationTarget.CLASS, AnnotationTarget.FUNCTION, AnnotationTarget.PROPERTY)
annotation class InternalArg2pApi
