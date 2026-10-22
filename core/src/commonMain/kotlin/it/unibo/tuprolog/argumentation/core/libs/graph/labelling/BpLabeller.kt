package it.unibo.tuprolog.argumentation.core.libs.graph.labelling

import it.unibo.tuprolog.argumentation.core.libs.PrologModule

object BpLabeller : PrologModule("prolog.argumentation.graph.labelling.bpgrounded", "bp_grounded") {
    override val prologRawTheory: String =
        """
        %==============================================================================
        % BP LABELLING [ICAIL]
        %==============================================================================

        % Query mode: not known to be directional (the burdens of proof order the arguments along the attacks), the
        % label of an argument is taken to depend on the arguments connected to it, in either direction
        locality(connected).

        argumentLabelling :-
            reifyBurdenOfProofs,
            filterBpDefeats,
            findall(X, context_check(argument(X)), Arguments),
            bpLabelling(Arguments).

        bpLabelling(Arguments) :-
            member(A, Arguments),
            grounded_old::allAttacksOUT(A), !,
            context_assert(in(A)),
            utils::subtract(Arguments, [A], NewArguments),
            bpLabelling(NewArguments).
        bpLabelling(Arguments) :-
            member(A, Arguments),
            \+ isArgumentInBurdenOfProof(A),
            grounded_old::oneAttackIN(A), !,
            context_assert(out(A)),
            utils::subtract(Arguments, [A], NewArguments),
            bpLabelling(NewArguments).
        bpLabelling(Arguments) :-
            context_active(Branch),
            mostGroundedBpUnd(Arguments, A),
            context_branch(Branch, _),
            context_assert(out(A)),
            utils::subtract(Arguments, [A], NewArguments),
            bpLabelling(NewArguments).
        bpLabelling(Args) :-
            notLabelled(Args),
            findall(_, (member(A, Args), context_assert(und(A))), _).

        notLabelled(Args) :-
            \+ (
                member(A, Args),
                (
                    context_check(in(A));
                    context_check(out(A));
                    context_check(und(A))
                )
            ).


        %==============================================================================
        % BP LABELLING UTILITIES
        %==============================================================================

        isInBurdenOfProof(Conclusion) :-
            context_check(reifiedBp(Literals)),
            member(Conclusion, Literals), !.

        isArgumentInBurdenOfProof([_, _, Conclusion, _, _]) :-
            isInBurdenOfProof(Conclusion).

        filterBpDefeats :-
            findall(_, (
                context_check(attack(T, A, B, C)),
                filterBpDefeat(T, A, B, C)
            ), _).

        filterBpDefeat(T, B, A, C) :-
            (T = rebut; T = undermine),
            isArgumentInBurdenOfProof(B),
            \+ superiority::superiorArgument(B, C),
            context_retract(attack(T, B, A, C)),
            context_id(argument(B), IdB),
            context_id(argument(A), IdA),
            context_id(argument(C), IdC),
            context_retract(attackId(T, IdB, IdA, IdC)).

        mostGroundedBpUnd(Arguments, Arg) :-
            member(Arg, Arguments),
            isArgumentInBurdenOfProof(Arg),
            \+ (
                member(A, Arguments), Arg \= A,
                isArgumentInBurdenOfProof(A),
                argumentChain(A, Arg)
            ).

        % argumentChain(+A, ?B): B is A or an argument reached from A through attacks; the arguments are visited breadth
        % first, each once (attack cycles are common: rebuttals are mutual)
        argumentChain(A, B) :- reaches([A], [], B), !.

        reaches([B|_], _, B) :- !.
        reaches([A|Frontier], Visited, B) :-
            findall(C, (
                context_check(attack(_, A, C, _)),
                \+ member(C, [A|Visited]),
                \+ member(C, Frontier)
            ), Next),
            utils::append_fast(Frontier, Next, Queue),
            reaches(Queue, [A|Visited], B).

        %==============================================================================
        % BURDEN OF PROOF REIFICATION
        %==============================================================================

        reifyBurdenOfProofs :-
            findall(_, (
                context_check(abstractBp(AbstractBp)),
                fillTemplate(AbstractBp, R),
                context_absent(reifiedBp(R)),
                context_assert(reifiedBp(R))
            ), _).

        /*
            Fill the template (first parameter) using arguments conclusions
        */

        fillTemplate([], []).
        fillTemplate([H|T], [[H]|R]) :-
            context_check(argument([_ , _, [P], _, _])),
            fillTemplate(T, R).
        """.trimIndent()
}
