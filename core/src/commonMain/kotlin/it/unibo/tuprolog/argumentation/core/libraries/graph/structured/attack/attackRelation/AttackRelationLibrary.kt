package it.unibo.tuprolog.argumentation.core.libraries.graph.structured.attack.attackRelation

import it.unibo.tuprolog.argumentation.core.libraries.PrologLibrary
import it.unibo.tuprolog.argumentation.core.libraries.graph.structured.attack.attackRelation.prolog.THEORY

/**
 * When an argument attacks another one directly (module `attackRelation`): rebutting or undermining it through a conflict
 * between their conclusions, rebutting or undermining it through a contrary premise, or undercutting its top rule.
 * Whether the attack succeeds is up to the graph extensions (preferences, restrictions).
 */
object AttackRelationLibrary : PrologLibrary("attackRelation") {
    override val prologRawTheory: String = THEORY
}
