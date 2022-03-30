package it.unibo.tuprolog.argumentation.actor.libs

import it.unibo.tuprolog.argumentation.core.libs.ArgLibrary
import it.unibo.tuprolog.argumentation.core.libs.ArgsFlag
import it.unibo.tuprolog.argumentation.core.libs.Loadable
import it.unibo.tuprolog.solve.library.AliasedLibrary
import it.unibo.tuprolog.solve.library.Library
import it.unibo.tuprolog.theory.Theory
import it.unibo.tuprolog.theory.parsing.parse

object TheoryChainer : ArgLibrary, Loadable {

    override val alias = "prolog.argumentation.actor.helper"

    override val baseContent: AliasedLibrary
        get() = Library.aliased(
            alias = this.alias,
            theory = Theory.parse(
                """            
                chainable(X) :-
                    member(rl(_) :- [_, Prem, _], X),
                    tuple_to_list(Prem, LPrem),
                    ff(LPrem, LLPrem),
                    member(P, LLPrem),
                    context_check(clause(rl(P), _)).
                    
                chainable(X) :- 
                    member(rl(Conclusion) :- [_, _, Conclusion], X),
                    context_check(clause(rl(_), [_, Prem, _])),
                    tuple_to_list(Prem, LPrem),
                    ff(LPrem, LLPrem),
                    member(Conclusion, LLPrem).
                    
                chainable(X) :- 
                    member(rl(Conclusion) :- [_, Conclusion], X),
                    context_check(clause(rl(_), [_, Prem, _])),
                    tuple_to_list(Prem, LPrem),
                    ff(LPrem, LLPrem),
                    member(Conclusion, LLPrem).
                    
                tuple_to_list(A,[A]) :- nonvar(A), A \= (_ , _), !.
                tuple_to_list((A,B),L) :-
                    tuple_to_list(A, La),
                    tuple_to_list(B, Lb),
                    append(La, Lb,L).

                ff([[]], []) :- !.
                ff([[X]], [X]) :- !.
                ff(X, X).
                """.trimIndent()
            )
        )

    override val baseFlags: Iterable<ArgsFlag<*, *>>
        get() = emptyList()

    override fun identifier(): String = "chainer"
}
