package it.unibo.tuprolog.argumentation.core.libraries.graph.structured.ruleGraph

import it.unibo.tuprolog.argumentation.core.libraries.PrologLibrary
import it.unibo.tuprolog.argumentation.core.libraries.graph.structured.ruleGraph.prolog.THEORY

/**
 * The whole argumentation graph of the rules of a theory (module `ruleGraph`, the graphBuilder of the profiles but
 * abstract, see [it.unibo.tuprolog.argumentation.core.libraries.framework.settings.helpers.Profiles]): every argument its rules make (see the module argumentBuilder) and the
 * attacks between them (attackBuilder), saved in the context (see the module store). The arguments derived from
 * them are added when the graph is changed (see phases::modifyGraph/0).
 */
object RuleGraphLibrary : PrologLibrary("ruleGraph") {
    override val prologRawTheory: String = THEORY
}
