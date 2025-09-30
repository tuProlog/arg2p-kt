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
        argumentLabelling :-
            findall(_, naive:::argumentLabelling, _),
            findall((In, Out, Und, Branch), (
                cache_check(naive(In, Out, Und, Branch))
            ), Results),
            filter(Results, Results, Filtered),
            member(X, Filtered),
            context_checkout(X).
        
        filter([], _, []).
        filter([(_, _, Und, Branch)|T], All, [Branch|RT]) :-
            check_minimal(Und, All), !,
            filter(T, All, RT).
        filter([_|T], All, RT) :-
            filter(T, All, RT).
            
        check_minimal(Und, All) :-
            \+ (
                member((_, _, X, _), All),
                Und \== X,
                \+ (
                    member(Z, X),
                    \+ member(Z, Und)
                )
            ).
        """.trimIndent()
}
