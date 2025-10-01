package it.unibo.tuprolog.argumentation.core.libs.graph.labelling

import it.unibo.tuprolog.argumentation.core.libs.ArgLibrary
import it.unibo.tuprolog.argumentation.core.libs.ArgsFlag
import it.unibo.tuprolog.argumentation.core.libs.LazyRawPrologContent
import it.unibo.tuprolog.argumentation.core.libs.Loadable
import it.unibo.tuprolog.argumentation.core.libs.basic.DynamicLoader
import it.unibo.tuprolog.core.operators.OperatorSet
import it.unibo.tuprolog.solve.library.Library

object PreferredLabeller :
    LazyRawPrologContent(),
    ArgLibrary,
    Loadable {
    override val alias = "prolog.argumentation.graph.labelling.preferred"

    override val baseContent: Library
        get() =
            Library.of(
                alias = this.alias,
                clauses = this.prologTheory,
            )
    override val baseFlags: Iterable<ArgsFlag<*, *>>
        get() = emptyList()

    override fun identifier(): String = "preferred"

    override val theoryOperators =
        DynamicLoader
            .operators()
            .plus(OperatorSet.DEFAULT)

    override val prologRawTheory: String =
        """
        argumentLabelling :-
            findall(_, complete:::argumentLabelling, _),
            findall((In, Out, Und, Branch), (
                cache_check(complete(In, Out, Und, Branch))
            ), Results),
            filter(Results, Results, Filtered),
            member(X, Filtered),
            context_checkout(X).
        
        filter([], _, []).
        filter([(In, _, _, Branch)|T], All, [Branch|RT]) :-
            check_maximal(In, All), !,
            filter(T, All, RT).
        filter([_|T], All, RT) :-
            filter(T, All, RT).
            
        check_maximal(In, All) :-
            \+ (
                member((X, _, _, _), All),
                In \== X,
                \+ (
                    member(Y, In), 
                    \+ member(Y, X)
                )
            ).
        """.trimIndent()
}
