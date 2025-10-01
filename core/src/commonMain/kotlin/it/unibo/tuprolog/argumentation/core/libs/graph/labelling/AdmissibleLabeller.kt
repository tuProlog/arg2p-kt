package it.unibo.tuprolog.argumentation.core.libs.graph.labelling

import it.unibo.tuprolog.argumentation.core.libs.ArgLibrary
import it.unibo.tuprolog.argumentation.core.libs.ArgsFlag
import it.unibo.tuprolog.argumentation.core.libs.LazyRawPrologContent
import it.unibo.tuprolog.argumentation.core.libs.Loadable
import it.unibo.tuprolog.argumentation.core.libs.basic.DynamicLoader
import it.unibo.tuprolog.core.operators.OperatorSet
import it.unibo.tuprolog.solve.library.Library

object AdmissibleLabeller :
    LazyRawPrologContent(),
    ArgLibrary,
    Loadable {
    override val alias = "prolog.argumentation.graph.labelling.admissible"

    override val baseContent: Library
        get() =
            Library.of(
                alias = this.alias,
                clauses = this.prologTheory,
            )
    override val baseFlags: Iterable<ArgsFlag<*, *>>
        get() = emptyList()

    override fun identifier(): String = "admissible"

    override val theoryOperators =
        DynamicLoader
            .operators()
            .plus(OperatorSet.DEFAULT)

    override val prologRawTheory: String =
        """
        argumentLabelling :-
            conflictfree::argumentLabelling,
            admissibleSet.
        
        admissibleSet :-
            \+ (context_check(inId(H)), \+ admissible(H)).
        admissible(H) :-
            \+ (
                context_check(clause(att(Attacker, H), _)),
                \+ (
                    context_check(clause(att(Defendant, Attacker), _)),
                    context_check(inId(Defendant)),
                    Defendant \= Attacker
                )
            ).
        """.trimIndent()
}
