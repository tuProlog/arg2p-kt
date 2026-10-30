package it.unibo.tuprolog.argumentation.core.modules.graph

import it.unibo.tuprolog.argumentation.core.modules.PrologModule

/**
 * The whole argumentation graph of the rules of a theory (module `ruleGraph`, the graphBuilder of the profiles but
 * abstract, see [it.unibo.tuprolog.argumentation.core.runtime.settings.Profiles]): every argument its rules make (see the module argumentBuilder), those derived from them (conflictArguments), and the
 * attacks between them (attackBuilder), saved in the context (see the module graphStore).
 */
object RuleGraphModule : PrologModule("prolog.argumentation.graph", "ruleGraph") {
    override val prologRawTheory: String =
        // language=Prolog
        """
        %! buildArgumentationGraph
        %  The arguments of the theory, those derived from them, and the attacks between them.
        buildArgumentationGraph :-
            buildArguments,
            buildAdditionalArguments,
            buildAttacks.

        %! buildArguments
        %  The arguments of the theory.
        buildArguments :-
            argumentBuilder::buildArguments.

        %! buildAdditionalArguments
        %  The arguments derived from the others: (constraints) the instances of those holding for sets of values, then
        %  (conflictArguments) the copies for the conflicts.
        buildAdditionalArguments :-
            (parser::instances -> constraints::buildAdditionalArguments ; true),
            conflictArguments::buildAdditionalArguments.

        %! buildAttacks
        %  The attacks between the saved arguments.
        buildAttacks :-
            attackBuilder::buildAttacks.
        """.trimIndent()
}
