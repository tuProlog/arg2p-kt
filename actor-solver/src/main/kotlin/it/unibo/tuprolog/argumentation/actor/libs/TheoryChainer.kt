package it.unibo.tuprolog.argumentation.actor.libs

import it.unibo.tuprolog.argumentation.core.libs.ArgsFlag
import it.unibo.tuprolog.argumentation.core.libs.BaseArgLibrary
import it.unibo.tuprolog.argumentation.core.libs.Loadable
import it.unibo.tuprolog.solve.library.AliasedLibrary
import it.unibo.tuprolog.solve.library.Library
import it.unibo.tuprolog.theory.Theory
import it.unibo.tuprolog.theory.parsing.parse

object TheoryChainer : BaseArgLibrary(), Loadable {

    override val alias = "prolog.argumentation.actor.helper"

    override val baseContent: AliasedLibrary
        get() = Library.aliased(
            alias = this.alias,
            theory = Theory.parse(
                """            
                chainable(X) :-
                    member(rule([_, Prem, _]), X),
                    member(P, Prem),
                    (context_check(rule([_, _, P])); context_check(premise([_, P]))).
                    
                chainable(X) :- 
                    member(rule([_, _, Conclusion]), X),
                    context_check(rule([_, Prem, _])),
                    member(Conclusion, Prem).
                    
                chainable(X) :- 
                    member(premise([_, Conclusion]), X),
                    context_check(rule([_, Prem, _])),
                    member(Conclusion, Prem).
                """.trimIndent()
            )
        )

    override val baseFlags: Iterable<ArgsFlag<*, *>>
        get() = emptyList()

    override fun identifier(): String = "chainer"
}
