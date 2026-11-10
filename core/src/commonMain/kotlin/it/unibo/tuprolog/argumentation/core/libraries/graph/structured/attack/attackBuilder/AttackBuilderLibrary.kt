package it.unibo.tuprolog.argumentation.core.libraries.graph.structured.attack.attackBuilder

import it.unibo.tuprolog.argumentation.core.libraries.PrologLibrary
import it.unibo.tuprolog.argumentation.core.libraries.graph.structured.attack.attackBuilder.prolog.THEORY

/**
 * The attacks between the arguments of the graph (module `attackBuilder`): each argument against those that may
 * attack it (see the module attackSearch), as the module attackRelation states them, saved in the context (see
 * the module store, which reads them extended to the arguments built on the attacked one).
 */
object AttackBuilderLibrary : PrologLibrary("attackBuilder") {
    override val prologRawTheory: String = THEORY
}
