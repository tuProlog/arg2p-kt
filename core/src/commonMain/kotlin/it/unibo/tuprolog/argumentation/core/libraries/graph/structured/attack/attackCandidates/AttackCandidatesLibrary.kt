package it.unibo.tuprolog.argumentation.core.libraries.graph.structured.attack.attackCandidates

import it.unibo.tuprolog.argumentation.core.libraries.PrologLibrary
import it.unibo.tuprolog.argumentation.core.libraries.graph.structured.attack.attackCandidates.prolog.THEORY

/**
 * The statements whose arguments may attack an argument, or be attacked by it (module `attackCandidates`): the
 * graph builders only check the attacks (see the module attackRelation) between an argument and the arguments for these
 * statements, and query mode explores them.
 */
object AttackCandidatesLibrary : PrologLibrary("attackCandidates") {
    override val prologRawTheory: String = THEORY
}
