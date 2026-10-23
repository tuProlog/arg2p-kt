package it.unibo.tuprolog.argumentation.core.modules.labelling.argument.bp

import it.unibo.tuprolog.argumentation.core.modules.PrologModule

/**
 * The grounded labelling with burdens of persuasion (module `bp_grounded`, an argumentLabellingMode, ICAIL): as the
 * grounded one, but an argument for a statement under a burden of persuasion that is not IN is OUT rather than UND.
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
        %  The labelling, in the active context, the burdens of persuasion (abstractBp/1) reified first.
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
            mostGroundedBpUnd(Arguments, A),
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
            \+ ( graphStore::attack(_, B, A, _), \+ (graphStore::argumentLabel(out, B))).

        %! oneAttackIN(+Argument)
        %  An attacker of Argument is IN, or Argument attacks an IN argument.
        oneAttackIN(A) :-
            graphStore::attack(_, B, A, _),
            graphStore::argumentLabel(in, B), !.
        oneAttackIN(A) :-
            graphStore::attack(_, A, B, _),
            graphStore::argumentLabel(in, B), !.

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

        filterBpDefeats :-
            findall(_, (
                graphStore::attack(T, A, B, C),
                filterBpDefeat(T, A, B, C)
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
                argumentChain(A, Arg)
            ).

        %! argumentChain(+A, +B)
        %  B is A or an argument reached from A through attacks; the arguments are visited breadth
        %  first, each once (attack cycles are common: rebuttals are mutual)
        argumentChain(A, B) :- reaches([A], [], B), !.

        reaches([B|_], _, B) :- !.
        reaches([A|Frontier], Visited, B) :-
            findall(C, (
                graphStore::attack(_, A, C, _),
                \+ member(C, [A|Visited]),
                \+ member(C, Frontier)
            ), Next),
            utils::appendFast(Frontier, Next, Queue),
            reaches(Queue, [A|Visited], B).

        %==============================================================================
        % BURDEN OF PROOF REIFICATION
        %==============================================================================

        %! reifyBurdenOfProofs
        %  The burdens of persuasion of the theory (abstractBp/1) on the conclusions of the saved arguments, as reifiedBp/1 in the context.
        reifyBurdenOfProofs :-
            findall(_, (
                context_check(abstractBp(AbstractBp)),
                fillTemplate(AbstractBp, R),
                context_absent(reifiedBp(R)),
                context_assert(reifiedBp(R))
            ), _).

        % The literals of a burden, each the conclusion of a saved argument (instantiated jointly) unless it is ground
        fillTemplate([], []).
        fillTemplate([H|T], [[H]|R]) :-
            (ground(H) -> true ; argument::claim(Argument, H), graphStore::argument(Argument)),
            fillTemplate(T, R).
        """.trimIndent()
}
