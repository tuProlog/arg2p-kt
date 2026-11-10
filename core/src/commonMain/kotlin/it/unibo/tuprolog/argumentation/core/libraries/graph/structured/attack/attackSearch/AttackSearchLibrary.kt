package it.unibo.tuprolog.argumentation.core.libraries.graph.structured.attack.attackSearch

import it.unibo.tuprolog.argumentation.core.libraries.PrologLibrary
import it.unibo.tuprolog.argumentation.core.libraries.graph.structured.attack.attackSearch.prolog.THEORY

/**
 * The statements whose arguments may attack an argument, or be attacked by it (module `attackSearch`): the
 * graph builders only check the attacks (see the module attackRelation) between an argument and the arguments for these
 * statements, and query mode explores them.
 */
object AttackSearchLibrary : PrologLibrary("attackSearch") {
    override val prologRawTheory: String = THEORY
}
