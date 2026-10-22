package it.unibo.tuprolog.argumentation.core.modules.labelling.argument

import it.unibo.tuprolog.argumentation.core.libs.PrologModule

/**
 * The grounded labelling in Prolog (module `grounded_old`, an argumentLabellingMode), on the arguments rather
 * than their identifiers: the labellings with burdens of proof build on it.
 */
object GroundedLabeller : PrologModule("prolog.argumentation.graph.labelling.grounded.old", "grounded_old") {
    override val prologRawTheory: String =
        """
        %! locality(-Locality)
        %  Query mode: the label of an argument only depends on the arguments it depends on (directionality)
        locality(directional).

        %! argumentLabelling
        %  The grounded labelling of the saved arguments, in the active context.
        argumentLabelling :-
            graphStore::arguments(Arguments),
            groundedLabelling(Arguments).

        %! groundedLabelling(+Arguments)
        %  The grounded labelling of Arguments, the arguments not labelled yet.
        groundedLabelling(UND) :-
            member(A, UND),
            allAttacksOUT(A), !,
            utils::subtract(UND, [A], NewUND),
            graphStore::labelArgument(in, A),
            groundedLabelling(NewUND).
        groundedLabelling(UND) :-
            member(A, UND),
            oneAttackIN(A), !,
            utils::subtract(UND, [A], NewUND),
            graphStore::labelArgument(out, A),
            groundedLabelling(NewUND).
        groundedLabelling(Args) :- findall(_, (member(A, Args), graphStore::labelArgument(und, A)), _).

        % If an attack exists, it should come from an OUT argument

        %! allAttacksOUT(+Argument)
        allAttacksOUT(A) :-
            \+ ( graphStore::attack(_, B, A, _), \+ (graphStore::argumentLabel(out, B))).

        % Find an attack, if exists, from an IN argument, then ends

        %! oneAttackIN(+Argument)
        oneAttackIN(A) :-
            graphStore::attack(_, B, A, _),
            graphStore::argumentLabel(in, B), !.

        % If A attacks an IN argument, then A is OUT

        oneAttackIN(A) :-
            graphStore::attack(_, A, B, _),
            graphStore::argumentLabel(in, B), !.
        """.trimIndent()
}
