package it.unibo.tuprolog.argumentation.actor.libs

import it.unibo.tuprolog.argumentation.core.flags.ArgsFlag
import it.unibo.tuprolog.argumentation.core.modules.ArgLibrary
import it.unibo.tuprolog.argumentation.core.modules.Loadable
import it.unibo.tuprolog.solve.library.Library
import it.unibo.tuprolog.theory.Theory
import it.unibo.tuprolog.theory.parsing.parse

internal object TheoryChainer : ArgLibrary, Loadable {
    override val alias = "prolog.argumentation.actor.helper"

    override val baseContent: Library
        get() =
            Library.of(
                alias = this.alias,
                clauses =
                    Theory.parse(
                        // language=Prolog
                        """            
                        chainable(X) :-
                            member(rl(_) :- [_, Prem, _], X),
                            tupleToList(Prem, LPrem),
                            ff(LPrem, LLPrem),
                            member(P, LLPrem),
                            context_check(clause(rl(P), _)).
                            
                        chainable(X) :- 
                            member(rl(Conclusion) :- [_, _, Conclusion], X),
                            context_check(clause(rl(_), [_, Prem, _])),
                            tupleToList(Prem, LPrem),
                            ff(LPrem, LLPrem),
                            member(Conclusion, LLPrem).
                            
                        chainable(X) :- 
                            member(rl(Conclusion) :- [_, Conclusion], X),
                            context_check(clause(rl(_), [_, Prem, _])),
                            tupleToList(Prem, LPrem),
                            ff(LPrem, LLPrem),
                            member(Conclusion, LLPrem).
                            
                        tupleToList(A,[A]) :- nonvar(A), A \= (_ , _), !.
                        tupleToList((A,B),L) :-
                            tupleToList(A, La),
                            tupleToList(B, Lb),
                            append(La, Lb,L).

                        ff([[]], []) :- !.
                        ff([[X]], [X]) :- !.
                        ff(X, X).
                        """.trimIndent(),
                    ),
            )

    override val baseFlags: Iterable<ArgsFlag<*, *>>
        get() = emptyList()

    override fun identifier(): String = "chainer"
}
