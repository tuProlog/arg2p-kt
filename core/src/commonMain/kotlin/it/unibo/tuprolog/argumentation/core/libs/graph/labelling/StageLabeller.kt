package it.unibo.tuprolog.argumentation.core.libs.graph.labelling

import it.unibo.tuprolog.argumentation.core.libs.ArgLibrary
import it.unibo.tuprolog.argumentation.core.libs.ArgsFlag
import it.unibo.tuprolog.argumentation.core.libs.LazyRawPrologContent
import it.unibo.tuprolog.argumentation.core.libs.Loadable
import it.unibo.tuprolog.argumentation.core.libs.basic.DynamicLoader
import it.unibo.tuprolog.core.operators.OperatorSet
import it.unibo.tuprolog.solve.library.Library

object StageLabeller :
    LazyRawPrologContent(),
    ArgLibrary,
    Loadable {
    override val alias = "prolog.argumentation.graph.labelling.stage"

    override val baseContent: Library
        get() =
            Library.of(
                alias = this.alias,
                clauses = this.prologTheory,
            )
    override val baseFlags: Iterable<ArgsFlag<*, *>>
        get() = emptyList()

    override fun identifier(): String = "stage"

    override val theoryOperators =
        DynamicLoader
            .operators()
            .plus(OperatorSet.DEFAULT)

    override val prologRawTheory: String =
        """
        % Query mode: not directional, but the labellings of unconnected parts of the graph are independent: the label
        % of an argument depends on the arguments connected to it, in either direction
        locality(connected).

        % Stage labellings are the naive ones with a minimal set of UND arguments
        argumentLabelling :-
            context_active(Branch),
            findall(Und-In, (
                naive:::argumentLabelling,
                findall(A, context_check(undId(A)), Und),
                findall(A, context_check(inId(A)), In)
            ), Naive),
            utils::minimal_by_set(Naive, Stage),
            utils::element(In, Stage),
            conflictfree::labelSet(Branch, In).
        """.trimIndent()
}
