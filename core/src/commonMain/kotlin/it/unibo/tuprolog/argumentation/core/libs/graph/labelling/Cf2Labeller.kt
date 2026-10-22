package it.unibo.tuprolog.argumentation.core.libs.graph.labelling

import it.unibo.tuprolog.argumentation.core.libs.PrologModule

/**
 * The cf2 labellings (module `cf2`, an argumentLabellingMode): the naive labellings of each strongly connected
 * component, the components taken in turn once the arguments attacked by the previous ones are suppressed.
 */
object Cf2Labeller : PrologModule("prolog.argumentation.graph.labelling.cf2", "cf2") {
    override val prologRawTheory: String =
        """
        %! locality(-Locality)
        %  Query mode: the label of an argument only depends on the arguments it depends on (directionality)
        locality(directional).

        %! argumentLabelling
        %  Each cf2 labelling in turn, left as the active context.
        argumentLabelling :-
            compute(naive).
        
        %! compute(+Base)
        %  Each labelling built with the labellings Base (naive or stage) of each component, left as the active context.
        compute(Base) :-
            graphStore::argumentIds(Args),
            context_active(Branch),
            context_branch(Branch, NewBranch),
            graphStore::excludeArguments,
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
                once((graphStore::attacksById(Y, X), graphStore::argumentLabelById(in, Y)))
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
            graphStore::argumentByIdIn(Branch, H, HA),
            graphStore::includeArgument(H, HA),
            moveArgs(T, Branch).
        """.trimIndent()
}
