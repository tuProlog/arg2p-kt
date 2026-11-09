package it.unibo.tuprolog.argumentation.actor.libraries.theoryChainer

import it.unibo.tuprolog.argumentation.actor.libraries.theoryChainer.prolog.THEORY
import it.unibo.tuprolog.argumentation.core.libraries.ArgLibrary
import it.unibo.tuprolog.argumentation.core.libraries.ArgsFlag
import it.unibo.tuprolog.argumentation.core.libraries.Loadable
import it.unibo.tuprolog.argumentation.core.libraries.framework.loader.notation.ModuleNotation
import it.unibo.tuprolog.solve.library.Library
import it.unibo.tuprolog.theory.Theory
import it.unibo.tuprolog.theory.parsing.parse

internal object TheoryChainer : ArgLibrary, Loadable {
    override val alias = "prolog.argumentation.actor.helper"

    override val baseContent: Library
        get() =
            Library.of(
                alias = this.alias,
                clauses =
                    Theory.parse(
                        THEORY,
                        ModuleNotation.theories,
                    ),
            )

    override val baseFlags: Iterable<ArgsFlag>
        get() = emptyList()

    override fun identifier(): String = "chainer"
}
