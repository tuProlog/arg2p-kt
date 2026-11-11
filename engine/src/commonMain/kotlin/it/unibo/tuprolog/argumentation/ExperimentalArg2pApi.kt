package it.unibo.tuprolog.argumentation

/**
 * A part of arg2p still experimental: the parallel solvers and their federation, the profiles of ABA. Unlike the rest of the
 * API it may change in a minor version, as it settles. Its users opt in (`@OptIn(ExperimentalArg2pApi::class)`, or
 * `-opt-in=it.unibo.tuprolog.argumentation.ExperimentalArg2pApi` for a whole module), warned otherwise; the modules of arg2p
 * do (see ktCompilerArgs in gradle.properties).
 */
@RequiresOptIn(level = RequiresOptIn.Level.WARNING, message = "Experimental in arg2p: it may change in a minor version")
@Retention(AnnotationRetention.BINARY)
@Target(
    AnnotationTarget.CLASS,
    AnnotationTarget.FUNCTION,
    AnnotationTarget.PROPERTY,
    AnnotationTarget.CONSTRUCTOR,
    AnnotationTarget.TYPEALIAS,
    AnnotationTarget.FIELD,
)
annotation class ExperimentalArg2pApi
