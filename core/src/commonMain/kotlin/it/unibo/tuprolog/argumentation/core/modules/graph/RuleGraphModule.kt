package it.unibo.tuprolog.argumentation.core.modules.graph

import it.unibo.tuprolog.argumentation.core.modules.PrologModule

/**
 * The whole argumentation graph of the rules of a theory (module `ruleGraph`, the graphBuilder of the profiles but
 * abstract, see [it.unibo.tuprolog.argumentation.core.runtime.settings.Profiles]): every argument its rules make (see the module argumentBuilder) and the
 * attacks between them (attackBuilder), saved in the context (see the module graphStore). The arguments derived from
 * them are added when the graph is changed (see structuredWhole::modifyGraph/0).
 */
object RuleGraphModule : PrologModule("prolog.argumentation.graph", "ruleGraph") {
    override val prologRawTheory: String =
        // language=Prolog
        """
        %! buildArgumentationGraph
        %  The arguments of the theory and the attacks between them.
        buildArgumentationGraph :-
            buildArguments,
            buildAttacks.

        %! buildArguments
        %  The arguments of the theory.
        buildArguments :-
            argumentBuilder::buildArguments.

        %! buildAdditionalArguments
        %  None, in query mode: the exploration builds the arguments of the rules, and those derived from them are added
        %  when the graph is changed (see structuredWhole::modifyGraph/0).
        buildAdditionalArguments.

        %! buildAttacks
        %  The attacks between the saved arguments.
        buildAttacks :-
            attackBuilder::buildAttacks.
        """.trimIndent()
}
