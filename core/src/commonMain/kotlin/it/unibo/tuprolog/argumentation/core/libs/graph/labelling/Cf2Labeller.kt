package it.unibo.tuprolog.argumentation.core.libs.graph.labelling

import it.unibo.tuprolog.argumentation.core.libs.PrologModule

object Cf2Labeller : PrologModule("prolog.argumentation.graph.labelling.cf2", "cf2") {
    override val prologRawTheory: String =
        """
        % Query mode: the label of an argument only depends on the arguments it depends on (directionality)
        locality(directional).

        argumentLabelling :-
            compute(naive).
        
        compute(Base) :-
            findall(A, context_check(clause(arg(A), _)), Args),
            context_active(Branch),
            context_branch(Branch, NewBranch),
            context_retract(arg(_) :- argument(_)),
            (
                computeIteratively(Base, Branch, Args, [])
            ;
                % the branches are released on backtracking, as the labellings built on them
                context_checkout(Branch),
                context_drop(NewBranch),
                fail
            ).

        % Args still to be evaluated, Suppressed already attacked by the extension (moved with the next step)
        computeIteratively(_, _, [], []).
        computeIteratively(Base, OriginBranch, [], [S|Suppressed]) :-
            evaluate(Base, OriginBranch, [S|Suppressed]).
        computeIteratively(Base, OriginBranch, [A|Args], Suppressed) :-
            % the first component in topological order is not attacked by the other ones
            utils::sccs([A|Args], [Next|_]),
            utils::appendLists([Suppressed, Next], ToEvaluate),
            evaluate(Base, OriginBranch, ToEvaluate),
            utils::subtract([A|Args], Next, Others),
            findall(X, (
                member(X, Others),
                once((context_check(clause(att(Y, X), _)), context_check(inId(Y))))
            ), NewSuppressed),
            utils::subtract(Others, NewSuppressed, Remaining),
            computeIteratively(Base, OriginBranch, Remaining, NewSuppressed).

        evaluate(Base, OriginBranch, Args) :-
            context_active(Branch),
            context_branch(Branch, NewBranch),
            (
                moveArgs(Args, OriginBranch),
                Base::argumentLabelling
            ;
                context_checkout(Branch),
                context_drop(NewBranch),
                fail
            ).
        
        moveArgs([], _).
        moveArgs([H|T], Branch) :-
            context_check(Branch, clause(arg(H), argument(HA))),
            context_assert(arg(H) :- argument(HA)),
            moveArgs(T, Branch).
        """.trimIndent()
}
