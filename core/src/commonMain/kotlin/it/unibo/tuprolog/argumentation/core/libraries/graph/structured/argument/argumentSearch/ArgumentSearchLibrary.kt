package it.unibo.tuprolog.argumentation.core.libraries.graph.structured.argument.argumentSearch

import it.unibo.tuprolog.argumentation.core.libraries.PrologLibrary
import it.unibo.tuprolog.argumentation.core.libraries.graph.structured.argument.argumentSearch.prolog.THEORY

/**
 * The arguments for a statement, built on demand (module `argumentSearch`, for query mode): from a premise, or from a
 * rule whose premises are supported in turn by the arguments for them (explored through the module
 * queryGraph, which saves them).
 *
 * An argument is the same term as the graph builder makes (see the module argumentBuilder): its rules and info from
 * those of its sub-arguments (store::rulesAndInfo/5), its body the premises of its rule as written. Its parts are
 * collected in `found(Body, TopRule, Rules, Info, Supports)`, built in the clause bodies.
 */
object ArgumentSearchLibrary : PrologLibrary("argumentSearch") {
    override val prologRawTheory: String = THEORY
}
