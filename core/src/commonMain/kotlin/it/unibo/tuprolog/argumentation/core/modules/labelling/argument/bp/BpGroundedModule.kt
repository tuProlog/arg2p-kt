package it.unibo.tuprolog.argumentation.core.modules.labelling.argument.bp

import it.unibo.tuprolog.argumentation.core.modules.PrologModule

/**
 * The grounded labelling with burdens of persuasion (module `bp_grounded`, an argumentLabellingMode, ICAIL): as the
 * grounded one, but an argument for a statement under a burden of persuasion that is not IN is OUT rather than UND.
 * Part of the semantics, it changes the graph it labels: a rebut or undermine by an argument under a burden of
 * persuasion fails unless the argument is superior to the one attacked (see filterBpDefeats/0).
 */
object BpGroundedModule : PrologModule("prolog.argumentation.graph.labelling.bpgrounded", "bp_grounded") {
    override val prologRawTheory: String =
        // language=Prolog
        """
        %==============================================================================
        % BP LABELLING [ICAIL]
        %==============================================================================

        %! locality(-Locality)
        %  Query mode: not known to be directional (the burdens of proof order the arguments along the attacks), the
        %  label of an argument is taken to depend on the arguments connected to it, in either direction
        locality(connected).

        %! argumentLabelling
        %  The labelling, in the active context, the burdens of persuasion (abstractBp/1) reified first, and the
        %  attacks failing for them removed (see filterBpDefeats/0).
        argumentLabelling :-
            reifyBurdenOfProofs,
            filterBpDefeats,
            graphStore::arguments(Arguments),
            bpLabelling(Arguments).

        bpLabelling(Arguments) :-
            member(A, Arguments),
            allAttacksOUT(A), !,
            graphStore::labelArgument(in, A),
            utils::subtract(Arguments, [A], NewArguments),
            bpLabelling(NewArguments).
        bpLabelling(Arguments) :-
            member(A, Arguments),
            \+ isArgumentInBurdenOfProof(A),
            oneAttackIN(A), !,
            graphStore::labelArgument(out, A),
            utils::subtract(Arguments, [A], NewArguments),
            bpLabelling(NewArguments).
        bpLabelling(Arguments) :-
            context_active(Branch),
            mostGroundedBpUnd(Arguments, A), !,
            context_branch(Branch, _),
            graphStore::labelArgument(out, A),
            utils::subtract(Arguments, [A], NewArguments),
            bpLabelling(NewArguments).
        bpLabelling(Args) :-
            notLabelled(Args),
            findall(_, (member(A, Args), graphStore::labelArgument(und, A)), _).

        notLabelled(Args) :-
            \+ (
                member(A, Args),
                (
                    graphStore::argumentLabel(in, A);
                    graphStore::argumentLabel(out, A);
                    graphStore::argumentLabel(und, A)
                )
            ).


        %==============================================================================
        % BP LABELLING UTILITIES
        %==============================================================================

        %! groundedLabelling(+Arguments)
        %  The grounded labelling of Arguments, the arguments not labelled yet, on the labels of the others (the
        %  complete labelling with burdens of persuasion labels so the UND arguments of the partial one).
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

        %! allAttacksOUT(+Argument)
        %  Every attacker of Argument is OUT.
        allAttacksOUT(A) :-
            graphStore::argumentId(A, Id),
            \+ (graphStore::attacksById(B, Id), \+ graphStore::argumentLabelById(out, B)).

        %! oneAttackIN(+Argument)
        %  An attacker of Argument is IN, or Argument attacks an IN argument.
        oneAttackIN(A) :-
            graphStore::argumentId(A, Id),
            (graphStore::attacksById(B, Id) ; graphStore::attacksById(Id, B)),
            graphStore::argumentLabelById(in, B), !.

        %! isInBurdenOfProof(+Conclusion)
        %  A burden of persuasion is on Conclusion.
        isInBurdenOfProof(Conclusion) :-
            context_check(reifiedBp(Literals)),
            member(Conclusion, Literals), !.

        %! isArgumentInBurdenOfProof(+Argument)
        %  A burden of persuasion is on the conclusion of Argument.
        isArgumentInBurdenOfProof(Argument) :-
            argument::conclusion(Argument, Conclusion),
            isInBurdenOfProof(Conclusion).

        %! filterBpDefeats
        %  The rebuts and undermines by an argument under a burden of persuasion removed from the graph, unless it is
        %  superior to the argument attacked: a burden is met by winning, not by drawing.
        filterBpDefeats :-
            findall(_, (
                graphStore::directAttack(T, A, C),
                filterBpDefeat(T, A, C, C)
            ), _).

        filterBpDefeat(T, B, A, C) :-
            (T = rebut; T = undermine),
            isArgumentInBurdenOfProof(B),
            \+ superiority::superiorArgument(B, C),
            graphStore::removeAttack(T, B, A, C).

        mostGroundedBpUnd(Arguments, Arg) :-
            member(Arg, Arguments),
            isArgumentInBurdenOfProof(Arg),
            \+ (
                member(A, Arguments), Arg \= A,
                isArgumentInBurdenOfProof(A),
                graphStore::reaches(A, Arg)
            ).

        %==============================================================================
        % BURDEN OF PROOF REIFICATION
        %==============================================================================

        %! reifyBurdenOfProofs
        %  The burdens of persuasion of the theory (see parser::burden/1) on the conclusions of the saved arguments, as
        %  reifiedBp/1 in the context.
        reifyBurdenOfProofs :-
            findall(_, (
                parser::burden(AbstractBp),
                fillTemplate(AbstractBp, R),
                context_absent(reifiedBp(R)),
                context_assert(reifiedBp(R))
            ), _).

        % The literals of a burden, each the conclusion of a saved argument (instantiated jointly) unless it is ground
        fillTemplate([], []).
        fillTemplate([H|T], [[H]|R]) :-
            (ground(H) -> true ; graphStore::argumentFor([H], _)),
            fillTemplate(T, R).
        """.trimIndent()
}
