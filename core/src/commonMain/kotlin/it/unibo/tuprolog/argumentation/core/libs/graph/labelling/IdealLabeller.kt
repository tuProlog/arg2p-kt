package it.unibo.tuprolog.argumentation.core.libs.graph.labelling

import it.unibo.tuprolog.argumentation.core.libs.ArgLibrary
import it.unibo.tuprolog.argumentation.core.libs.ArgsFlag
import it.unibo.tuprolog.argumentation.core.libs.LazyRawPrologContent
import it.unibo.tuprolog.argumentation.core.libs.Loadable
import it.unibo.tuprolog.argumentation.core.libs.basic.DynamicLoader
import it.unibo.tuprolog.core.operators.OperatorSet
import it.unibo.tuprolog.solve.library.Library

object IdealLabeller :
    LazyRawPrologContent(),
    ArgLibrary,
    Loadable {
    override val alias = "prolog.argumentation.graph.labelling.ideal"

    override val baseContent: Library
        get() =
            Library.of(
                alias = this.alias,
                clauses = this.prologTheory,
            )
    override val baseFlags: Iterable<ArgsFlag<*, *>>
        get() = emptyList()

    override fun identifier(): String = "ideal"

    override val theoryOperators =
        DynamicLoader
            .operators()
            .plus(OperatorSet.DEFAULT)

    override val prologRawTheory: String =
        """
        % The ideal labelling has IN the maximal admissible subset of the intersection of the preferred extensions
        argumentLabelling :-
            context_active(Branch),
            findall(In, (
                preferred:::argumentLabelling,
                findall(A, context_check(inId(A)), In)
            ), Preferred),
            utils::intersection(Preferred, Common),
            admissible::maximal_admissible_subset(Common, Ideal),
            conflictfree::labelSet(Branch, Ideal).
        """.trimIndent()
}
