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
            context_active(Branch),
            naiveLabellingInMemory(Branch).
            
            
        naiveLabellingInMemory(Branch) :-
            findall(A, context_check(clause(arg(A), _)), Args),
            combinations(Args, TentativeSet),
            maximalConflictFreeSetInMemory(TentativeSet),
            context_branch(Branch, NewBranch),
            findall(_, (
                member(IdA, TentativeSet),    
                context_check(clause(arg(IdA), argument(A))),
                context_assert(in(A)),
                context_assert(inId(IdA))
            ), _),
            finalizeLabelling,
            utils::recoverArgumentLabellingId(In, Out, Und),
            \+ cache_check(naive(In, Out, Und, _)),
            cache_assert(naive(In, Out, Und, NewBranch)).
        
        combinations([], []).
        combinations([H|T], [H|RT]) :- combinations(T, RT).
        combinations([_|T], RT) :- combinations(T, RT).
            
        conflictFreeSetInMemory(TentativeSet) :-
            \+ (
                member(A, TentativeSet), 
                member(B, TentativeSet),
                context_check(clause(att(A, B), _))
            ).
            
        maximalConflictFreeSetInMemory(TentativeSet) :-
            conflictFreeSetInMemory(TentativeSet),
            \+ (
                context_check(clause(arg(IdX), _)),
                \+ member(IdX, TentativeSet),
                \+ (
                    (context_check(clause(att(IdX, IdY), _));context_check(clause(att(IdY, IdX), _))),
                    member(IdY, TentativeSet)
                )
            ).
            
        
        naiveLabelling(Branch) :-
            maximalConflictFreeSet,
            finalizeLabelling,
            utils::recoverArgumentLabellingId(In, Out, Und),
            \+ cache_check(naive(In, Out, Und, _)),
            cache_assert(naive(In, Out, Und, Branch)).
        naiveLabelling(Branch) :-
            argument_to_evaluate(Branch, X, IdX),
            context_branch(Branch, NewBranch),
            context_assert(in(X)),
            context_assert(inId(IdX)),
            naiveLabelling(NewBranch).
        
        
        argument_to_evaluate(X, IdX) :-
            context_active(Branch),
            argument_to_evaluate(Branch, X, IdX).    
        argument_to_evaluate(Branch, X, IdX) :-
            context_check(Branch, clause(arg(IdX), argument(X))),
            \+ context_check(Branch, outId(IdX)),
            \+ context_check(Branch, inId(IdX)).
        
        conflictFreeSet :-
            \+ (
                context_check(inId(A)), 
                context_check(inId(B)),
                context_check(clause(att(A, B), _))
            ).
            
        maximalConflictFreeSet :-
            conflictFreeSet,
            \+ (
                argument_to_evaluate(X, IdX),
                \+ (
                    (context_check(clause(att(IdX, IdY), _));context_check(clause(att(IdY, IdX), _))),
                    context_check(inId(IdY))
                )
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

        """.trimIndent()
}
