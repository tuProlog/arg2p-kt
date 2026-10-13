package it.unibo.tuprolog.argumentation.core.libs.graph.labelling

import it.unibo.tuprolog.argumentation.core.libs.ArgLibrary
import it.unibo.tuprolog.argumentation.core.libs.ArgsFlag
import it.unibo.tuprolog.argumentation.core.libs.LazyRawPrologContent
import it.unibo.tuprolog.argumentation.core.libs.Loadable
import it.unibo.tuprolog.argumentation.core.libs.basic.DynamicLoader
import it.unibo.tuprolog.core.operators.OperatorSet
import it.unibo.tuprolog.solve.library.Library

object StronglyAdmissibleLabeller :
    LazyRawPrologContent(),
    ArgLibrary,
    Loadable {
    override val alias = "prolog.argumentation.graph.labelling.stronglyadmissible"

    override val baseContent: Library
        get() =
            Library.of(
                alias = this.alias,
                clauses = this.prologTheory,
            )
    override val baseFlags: Iterable<ArgsFlag<*, *>>
        get() = emptyList()

    override fun identifier(): String = "stronglyadmissible"

    override val theoryOperators =
        DynamicLoader
            .operators()
            .plus(OperatorSet.DEFAULT)

    override val prologRawTheory: String =
        """
        argumentLabelling :-
            admissible::argumentLabelling,
            stronglyAdmissibleSet.
        
        % Every In argument must be reachable starting from the unattacked ones:
        % iteratively add the In arguments defended by those added so far
        stronglyAdmissibleSet :-
            findall(H, context_check(inId(H)), In),
            rooted([], In).

        rooted(_, []) :- !.
        rooted(Built, Pending) :-
            findall(H, (member(H, Pending), defendedBy(H, Built)), New),
            New \== [],
            utils::subtract(Pending, New, Rest),
            utils::appendLists([Built, New], NewBuilt),
            rooted(NewBuilt, Rest).

        defendedBy(H, Built) :-
            \+ (
                context_check(clause(att(Attacker, H), _)),
                \+ (
                    member(Defendant, Built),
                    context_check(clause(att(Defendant, Attacker), _))
                )
            ).
        """.trimIndent()
}
