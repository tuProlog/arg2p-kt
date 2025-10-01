package it.unibo.tuprolog.argumentation.core.libs.graph.labelling

import it.unibo.tuprolog.argumentation.core.libs.ArgLibrary
import it.unibo.tuprolog.argumentation.core.libs.ArgsFlag
import it.unibo.tuprolog.argumentation.core.libs.LazyRawPrologContent
import it.unibo.tuprolog.argumentation.core.libs.Loadable
import it.unibo.tuprolog.argumentation.core.libs.basic.DynamicLoader
import it.unibo.tuprolog.core.operators.OperatorSet
import it.unibo.tuprolog.solve.library.Library

object NaiveLabeller :
    LazyRawPrologContent(),
    ArgLibrary,
    Loadable {
    override val alias = "prolog.argumentation.graph.labelling.naive"

    override val baseContent: Library
        get() =
            Library.of(
                alias = this.alias,
                clauses = this.prologTheory,
            )
    override val baseFlags: Iterable<ArgsFlag<*, *>>
        get() = emptyList()

    override fun identifier(): String = "naive"

    override val theoryOperators =
        DynamicLoader
            .operators()
            .plus(OperatorSet.DEFAULT)

    override val prologRawTheory: String =
        """
        argumentLabelling :-
            cache_retract(naive(_, _, _, _)),
            conflictfree::argumentLabelling,
            maximalConflictFreeSet,
            utils::recoverArgumentLabellingId(In, Out, Und),
            \+ cache_check(naive(In, Out, Und, _)),
            context_active(Branch),
            cache_assert(naive(In, Out, Und, Branch)).
            
            
        maximalConflictFreeSet :-
            \+ (
                context_check(clause(arg(IdX), _)),
                \+ context_check(inId(IdX)),
                \+ (
                    (context_check(clause(att(IdX, IdY), _));context_check(clause(att(IdY, IdX), _))),
                    context_check(inId(IdY))
                )
            ).
        """.trimIndent()
}
