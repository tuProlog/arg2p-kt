package it.unibo.tuprolog.argumentation.core.libs.graph.labelling

import it.unibo.tuprolog.argumentation.core.libs.ArgLibrary
import it.unibo.tuprolog.argumentation.core.libs.ArgsFlag
import it.unibo.tuprolog.argumentation.core.libs.LazyRawPrologContent
import it.unibo.tuprolog.argumentation.core.libs.Loadable
import it.unibo.tuprolog.argumentation.core.libs.basic.DynamicLoader
import it.unibo.tuprolog.core.operators.OperatorSet
import it.unibo.tuprolog.solve.library.Library

object Cf2Labeller :
    LazyRawPrologContent(),
    ArgLibrary,
    Loadable {
    override val alias = "prolog.argumentation.graph.labelling.cf2"

    override val baseContent: Library
        get() =
            Library.of(
                alias = this.alias,
                clauses = this.prologTheory,
            )
    override val baseFlags: Iterable<ArgsFlag<*, *>>
        get() = emptyList()

    override fun identifier(): String = "cf2"

    override val theoryOperators =
        DynamicLoader
            .operators()
            .plus(OperatorSet.DEFAULT)

    override val prologRawTheory: String =
        """
        argumentLabelling :-
            compute(naive).
        
        compute(Base) :-
            findall(A, context_check(clause(arg(A), _)), Args),
            computeScc(Args, Scc),
            context_active(Branch),
            context_branch(Branch, NewBranch),
            context_retract(arg(_) :- argument(_)),
            computeIteratively(Base, Branch, Scc).
        
        computeIteratively(_, _, []).
        computeIteratively(Base, OriginBranch, Scc) :-
            once(getNext(Scc, Next, Others)),
            context_active(Branch),
            context_branch(Branch, NewBranch),
            moveArgs(Next, OriginBranch),
            Base::argumentLabelling,
            computeIteratively(Base, OriginBranch, Others).
        
        getNext(Scc, X, Others) :-
            member(X, Scc),
            utils::subtract(Scc, [X], Others),   
            utils::appendLists(Others, FO),
            \+ (
                member(Y, X),
                context_check(clause(att(A, Y), _)),
                member(A, FO)
            ).
          
        moveArgs([], _).
        moveArgs([H|T], Branch) :-
            context_check(Branch, clause(arg(H), argument(HA))),
            context_assert(arg(H) :- argument(HA)),
            moveArgs(T, Branch).
            
        computeScc(T, MScc) :-
            findall(R, scc(T, [], R), Scc),
            findall(S, (member(S, Scc), check_maximal(S, Scc)), MScc).
            
        check_maximal(S, All) :-
            \+ (
                member(S1, All),
                S \== S1,
                \+ (
                    member(Y, S), 
                    \+ member(Y, S1)
                )
            ).
        
        scc([], L, L) :- stronglyConnected(L).
        scc([H|Others], T, R) :- scc(Others, [H|T], R). 
        scc([_|Others], T, R) :- scc(Others, T, R).
        
        stronglyConnected(T) :-
            \+ (
               member(X, T),
               member(Y, T),
               \+ once(reach(X, Y, [X], T))
            ).
        
        reach(X, X, _, _) :- !.
        reach(X, Y, P, S) :- 
            context_check(clause(att(X, X1), _)),
            member(X1, S),
            \+ member(X1, P),
            reach(X1, Y, [X1|P], S).

        """.trimIndent()
}
