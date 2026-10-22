package it.unibo.tuprolog.argumentation.core.libs.graph.labelling

import it.unibo.tuprolog.argumentation.core.libs.ArgLibrary
import it.unibo.tuprolog.argumentation.core.libs.ArgsFlag
import it.unibo.tuprolog.argumentation.core.libs.LazyRawPrologContent
import it.unibo.tuprolog.argumentation.core.libs.Loadable
import it.unibo.tuprolog.argumentation.core.libs.basic.DynamicLoader
import it.unibo.tuprolog.core.operators.OperatorSet
import it.unibo.tuprolog.solve.library.Library

object EagerLabeller :
    LazyRawPrologContent(),
    ArgLibrary,
    Loadable {
    override val alias = "prolog.argumentation.graph.labelling.eager"

    override val baseContent: Library
        get() =
            Library.of(
                alias = this.alias,
                clauses = this.prologTheory,
            )
    override val baseFlags: Iterable<ArgsFlag<*, *>>
        get() = emptyList()

    override fun identifier(): String = "eager"

    override val theoryOperators =
        DynamicLoader
            .operators()
            .plus(OperatorSet.DEFAULT)

    override val prologRawTheory: String =
        """
        % Query mode: not directional, but the labellings of unconnected parts of the graph are independent: the label
        % of an argument depends on the arguments connected to it, in either direction
        locality(connected).

        % The eager labelling has IN the maximal admissible subset of the intersection of the semi-stable extensions
        argumentLabelling :-
            context_active(Branch),
            findall(In, (
                semistable:::argumentLabelling,
                findall(A, context_check(inId(A)), In)
            ), Semistable),
            utils::intersection(Semistable, Common),
            admissible::maximal_admissible_subset(Common, Eager),
            conflictfree::labelSet(Branch, Eager).
        """.trimIndent()
}
