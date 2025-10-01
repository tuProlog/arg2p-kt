package it.unibo.tuprolog.argumentation.core.libs.graph.labelling

import it.unibo.tuprolog.argumentation.core.libs.ArgLibrary
import it.unibo.tuprolog.argumentation.core.libs.ArgsFlag
import it.unibo.tuprolog.argumentation.core.libs.LazyRawPrologContent
import it.unibo.tuprolog.argumentation.core.libs.Loadable
import it.unibo.tuprolog.argumentation.core.libs.basic.DynamicLoader
import it.unibo.tuprolog.core.operators.OperatorSet
import it.unibo.tuprolog.solve.library.Library

object ConflictFreeLabeller :
    LazyRawPrologContent(),
    ArgLibrary,
    Loadable {
    override val alias = "prolog.argumentation.graph.labelling.conflictfree"

    override val baseContent: Library
        get() =
            Library.of(
                alias = this.alias,
                clauses = this.prologTheory,
            )
    override val baseFlags: Iterable<ArgsFlag<*, *>>
        get() = emptyList()

    override fun identifier(): String = "conflictfree"

    override val theoryOperators =
        DynamicLoader
            .operators()
            .plus(OperatorSet.DEFAULT)

    override val prologRawTheory: String =
        """
        argumentLabelling :-
            context_active(Branch),
            conflictFreeLabelling(Branch).
        
        conflictFreeLabelling(Branch) :-
            findall(A, context_check(clause(arg(A), _)), Args),
            combinations(Branch, Args, TentativeSet),
            conflictFreeSet(TentativeSet),
            context_branch(Branch, _),
            findall(_, (
                member(IdA, TentativeSet),    
                context_check(clause(arg(IdA), argument(A))),
                context_assert(in(A)),
                context_assert(inId(IdA))
            ), _),
            finalizeLabelling.
        
        combinations(_, [], []).
        combinations(Branch, [H|T], [H|RT]) :- combinations(Branch, T, RT).
        combinations(Branch, [H|T], RT) :- \+ context_check(Branch, inId(H)), combinations(Branch, T, RT).
        
        conflictFreeSet(TentativeSet) :-
            \+ (
                member(A, TentativeSet), 
                member(B, TentativeSet),
                context_check(clause(att(A, B), _))
            ).
            
        finalizeLabelling :-
            findall(_, (
               argument_to_evaluate(X, IdX),
               context_check(clause(att(IdY, IdX), _)),
               context_check(inId(IdY)),
               context_assert(out(X)),
               context_assert(outId(IdX))
            ), _),
            findall(_, (
               argument_to_evaluate(X, IdX),
               context_assert(und(X)),
               context_assert(undId(IdX))
            ), _).
            
        argument_to_evaluate(X, IdX) :-
            context_check(clause(arg(IdX), argument(X))),
            \+ context_check(outId(IdX)),
            \+ context_check(inId(IdX)).

        """.trimIndent()
}
