package it.unibo.tuprolog.argumentation.core.modules.graph

import it.unibo.tuprolog.argumentation.core.modules.PrologModule

/**
 * The whole argumentation graph of a theory (module `standard_af`, the graphBuildMode of the same name): every
 * argument its rules make (see the module argumentBuilder), those derived from them (conflictArguments), and the
 * attacks between them (attackBuilder), saved in the context (see the module graphStore).
 */
object StandardAfModule : PrologModule("prolog.argumentation.graph", "standard_af") {
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
        %  The arguments derived from the others (see the module conflictArguments).
        buildAdditionalArguments :-
            conflictArguments::buildAdditionalArguments.

        %! buildAttacks
        %  The attacks between the saved arguments.
        buildAttacks :-
            attackBuilder::buildAttacks.
        """.trimIndent()
}
