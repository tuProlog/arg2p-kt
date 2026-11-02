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
        %  The labelling of the abstract framework of Arguments and Attacks, in the context: the framework saved as the
        %  graph builder of abstract theories saves it (see abstractGraph::save/2), then labelled, with no extension (as
        %  the profile abstract has none).
        solve(Arguments, Attacks) :-
            context_reset,
            findall(att(X, Y), member((X, Y), Attacks), Atts),
            abstractGraph::save(Arguments, Atts),
            structuredWhole::buildArgumentLabelling.
        """.trimIndent()
}
