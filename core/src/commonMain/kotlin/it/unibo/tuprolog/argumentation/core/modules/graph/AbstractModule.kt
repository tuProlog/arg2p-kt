package it.unibo.tuprolog.argumentation.core.modules.graph

import it.unibo.tuprolog.argumentation.core.flags.ArgsFlag
import it.unibo.tuprolog.argumentation.core.flags.ArgumentLabellingMode
import it.unibo.tuprolog.argumentation.core.modules.PrologModule

/**
 * The labelling of abstract argumentation frameworks (module `abstract`): arguments and attacks given as they are,
 * labelled by the semantics of argumentLabellingMode.
 */
object AbstractModule : PrologModule("prolog.argumentation.abstract", "abstract") {
    override val baseFlags: Iterable<ArgsFlag<*, *>>
        get() = listOf(ArgumentLabellingMode)

    override val prologRawTheory: String =
        // language=Prolog
        """
        %! solve(+Arguments, +Attacks, -In, -Out, -Und)
        %  The labelling of the abstract framework of Arguments (atoms) and Attacks ((A, B) pairs), by argumentLabellingMode: the arguments IN, OUT and UND.
        solve(Arguments, Attacks, I, O, U) :-
            solve(Arguments, Attacks),
            graphStore::claimLabelling(I, O, U).

        %! solve(+Arguments, +Attacks)
        %  The labelling of the abstract framework of Arguments and Attacks, in the context.
        %  The lists are visited natively (forEach): as the goal of a findall, every step of the saving would visit them.
        %  The goals of forEach are solved in the module utils: they name the modules of their predicates.
        solve(Arguments, Attacks) :-
            context_reset,
            utils::forEach(A, Arguments, (
                abstract::abstractArgument(A, Argument),
                graphStore::saveArgument(graph, [A], Argument)
            )),
            utils::forEach((X, Y), Attacks, (
                abstract::abstractArgument(X, XX),
                abstract::abstractArgument(Y, YY),
                graphStore::saveAttack(abstract, XX, YY, YY)
            )),
            structuredWhole::buildArgumentLabelling.

        % The argument for the abstract argument A: the strict rule A alone, concluding A
        abstractArgument(A, Argument) :-
            argument::infoParts(Info, [], [], []),
            argument::new([A], A, [A], [], Info, Argument).
        """.trimIndent()
}
