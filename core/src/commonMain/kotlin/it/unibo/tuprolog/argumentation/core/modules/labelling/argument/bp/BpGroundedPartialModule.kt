package it.unibo.tuprolog.argumentation.core.modules.labelling.argument.bp

import it.unibo.tuprolog.argumentation.core.modules.PrologModule

/**
 * The partial labelling with burdens of persuasion (module `bp_grounded_partial`, an argumentLabellingMode, ICLP):
 * each argument labelled IN*, OUT* or UND* by the conditions of the burdens on its conclusion and its complement.
 */
object BpGroundedPartialModule : PrologModule("prolog.argumentation.graph.labelling.bpgroundedpartial", "bp_grounded_partial") {
    override val prologRawTheory: String =
        // language=Prolog
        """
        %==============================================================================
        % BP PARTIAL LABELLING [ICLP]
        %==============================================================================

        % TODO: handle branching and cuts (complements also)

        %! locality(-Locality)
        %  Query mode: not known to be directional (the burdens of proof order the arguments along the attacks), the
        %  label of an argument is taken to depend on the arguments connected to it, in either direction
        locality(connected).

        %! argumentLabelling
        %  The labelling, in the active context.
        argumentLabelling :-
            bp_grounded::reifyBurdenOfProofs,
            graphStore::arguments(Arguments),
            partialBpLabelling(Arguments), !.

        %! partialBpLabelling(+Arguments)
        %  The labels of Arguments, the most grounded first, deciding first the arguments the label of each depends on.
        partialBpLabelling([]).
        partialBpLabelling(Arguments) :-
            mostGroundedArgument(Arguments, A),
            demonstration(Arguments, A, [A], Evaluated),
            utils::subtract(Arguments, [Evaluated], NewArgs),
            partialBpLabelling(NewArgs).

        /*
            (a.i) BP(neg(φ)), and no argument B for neg(φ) such that A < B is IN*, and no A1,...An is OUT*
        */
        demonstration(Arguments, A, Resolving, A) :-
            isComplementInBurdenOfProof(A),
            \+ notEvaluatedSuperiorComplementArgument(Arguments, A, Resolving, _),
            \+ (
                superiorComplementArgument(A, B),
                graphStore::argumentLabel(in, B)
            ),
            \+ notEvaluatedSubArgument(Arguments, A, Resolving, _),
            \+ (
                graphStore::support(X, A),
                graphStore::argumentLabel(out, X)
            ),
            graphStore::labelArgument(in, A).

        /*
            (a.ii) not BP(neg(φ)) and every argument B for neg(φ) such that B(not <)A is OUT*, and every A1,...An is IN*
        */
        demonstration(Arguments, A, Resolving, A) :-
            \+ isComplementInBurdenOfProof(A),
            \+ notEvaluatedSuperiorOrEqualComplementArgument(Arguments, A, Resolving, _),
            \+ (
                superiorOrEqualComplementArgument(A, B),
                \+ graphStore::argumentLabel(out, B)
            ),
            \+ notEvaluatedSubArgument(Arguments, A, Resolving, _),
            \+ (
                graphStore::support(X, A),
                \+ graphStore::argumentLabel(in, X)
            ),
            graphStore::labelArgument(in, A).

        /*
            (b.i.1) BP(φ) and exists an argument B for neg(φ) such that B(not <)A is not OUT*
        */
        demonstration(Arguments, A, Resolving, A) :-
            bp_grounded::isArgumentInBurdenOfProof(A),
            \+ notEvaluatedSuperiorOrEqualComplementArgument(Arguments, A, Resolving, _),
            superiorOrEqualComplementArgument(A, B),
            \+ graphStore::argumentLabel(out, B),
            graphStore::labelArgument(out, A).

        /*
            (b.i.2) BP(φ) and exist one of A1,...An is not IN*
        */
        demonstration(Arguments, A, Resolving, A) :-
            bp_grounded::isArgumentInBurdenOfProof(A),
            \+ notEvaluatedSubArgument(Arguments, A, Resolving, _),
            graphStore::support(X, A),
            \+ graphStore::argumentLabel(in, X),
            graphStore::labelArgument(out, A).

        /*
            (b.ii.1) not BP(φ) and an argument B for neg(φ) such A < B is IN*
        */
        demonstration(Arguments, A, Resolving, A) :-
            \+ bp_grounded::isArgumentInBurdenOfProof(A),
            \+ notEvaluatedSuperiorComplementArgument(Arguments, A, Resolving, _),
            superiorComplementArgument(A, B),
            graphStore::argumentLabel(in, B),
            graphStore::labelArgument(out, A).

        /*
            (b.ii.2) not BP(φ) and one of A1,...An is OUT*
        */
        demonstration(Arguments, A, Resolving, A) :-
            \+ bp_grounded::isArgumentInBurdenOfProof(A),
            \+ notEvaluatedSubArgument(Arguments, A, Resolving, _),
            graphStore::support(X, A),
            graphStore::argumentLabel(out, X),
            graphStore::labelArgument(out, A).

        /*
            If above definitions do not apply try to decide on the arguments on which they depend
        */
        demonstration(Arguments, A, Resolving, Evaluated) :-
            isComplementInBurdenOfProof(A),
            notEvaluatedSuperiorComplementArgument(Arguments, A, Resolving, B),
            demonstration(Arguments, B, [B|Resolving], Evaluated).

        demonstration(Arguments, A, Resolving, Evaluated) :-
            isComplementInBurdenOfProof(A),
            \+ notEvaluatedSuperiorComplementArgument(Arguments, A, Resolving, _),
            \+ (
                superiorComplementArgument(A, X),
                graphStore::argumentLabel(in, X)
            ),
            notEvaluatedSubArgument(Arguments, A, Resolving, B),
            demonstration(Arguments, B, [B|Resolving], Evaluated).

        demonstration(Arguments, A, Resolving, Evaluated) :-
            \+ isComplementInBurdenOfProof(A),
            notEvaluatedSuperiorOrEqualComplementArgument(Arguments, A, Resolving, B),
            demonstration(Arguments, B, [B|Resolving], Evaluated).

        demonstration(Arguments, A, Resolving, Evaluated) :-
            \+ isComplementInBurdenOfProof(A),
            \+ notEvaluatedSuperiorOrEqualComplementArgument(Arguments, A, Resolving, _),
            \+ (
                superiorOrEqualComplementArgument(A, X),
                \+ graphStore::argumentLabel(out, X)
            ),
            notEvaluatedSubArgument(Arguments, A, Resolving, B),
            demonstration(Arguments, B, [B|Resolving], Evaluated).

        demonstration(Arguments, A, Resolving, Evaluated) :-
            bp_grounded::isArgumentInBurdenOfProof(A),
            notEvaluatedSuperiorOrEqualComplementArgument(Arguments, A, Resolving, B),
            demonstration(Arguments, B, [B|Resolving], Evaluated).

        demonstration(Arguments, A, Resolving, Evaluated) :-
            bp_grounded::isArgumentInBurdenOfProof(A),
            notEvaluatedSubArgument(Arguments, A, Resolving, B),
            demonstration(Arguments, B, [B|Resolving], Evaluated).

        demonstration(Arguments, A, Resolving, Evaluated) :-
            \+ bp_grounded::isArgumentInBurdenOfProof(A),
            notEvaluatedSuperiorOrEqualComplementArgument(Arguments, A, Resolving, B),
            demonstration(Arguments, B, [B|Resolving], Evaluated).

        demonstration(Arguments, A, Resolving, Evaluated) :-
            \+ bp_grounded::isArgumentInBurdenOfProof(A),
            notEvaluatedSubArgument(Arguments, A, Resolving, B),
            demonstration(Arguments, B, [B|Resolving], Evaluated).

        /*
            (c) A is labelled UND* otherwise.
        */
        demonstration(Arguments, A, Resolving, A) :-
            graphStore::labelArgument(und, A).

        %==============================================================================
        % BP LABELLING UTILITIES
        %==============================================================================

        isComplementInBurdenOfProof(A) :-
            complement(A, Complement),
            bp_grounded::isInBurdenOfProof(Complement), !.

        mostGroundedArgument([], []).
        mostGroundedArgument([X], X).
        mostGroundedArgument([A|T], Best) :-
            mostGroundedArgument(T, Best),
            argument::rules(A, RulesA),
            argument::rules(Best, RulesBest),
            length(RulesA, LN1),
            length(RulesBest, LN2),
            LN1 > LN2, !.
        mostGroundedArgument([A|_], A).

        /*
            Get a conclusion complement
        */
        complement(Argument, Complement) :-
            argument::conclusion(Argument, Conclusion),
            conflict::between(Conclusion, Complement).

        notEvaluatedSubArgument(Arguments, A, Resolving, B) :-
            graphStore::support(B, A),
            member(B, Arguments),
            \+ member(B, Resolving).

        notEvaluatedSuperiorComplementArgument(Arguments, A, Resolving, B) :-
            superiorComplementArgument(A, B),
            member(B, Arguments),
            \+ member(B, Resolving).

        notEvaluatedSuperiorOrEqualComplementArgument(Arguments, A, Resolving, B) :-
            superiorOrEqualComplementArgument(A, B),
            member(B, Arguments),
            \+ member(B, Resolving).

        superiorComplementArgument(A, B) :-
            complementArgument(A, B),
            superiority::superiorArgument(B, A).

        superiorOrEqualComplementArgument(A, B) :-
            complementArgument(A, B),
            \+ superiority::superiorArgument(A, B).

        % B is a saved argument for the complement of the conclusion of A
        complementArgument(A, B) :-
            complement(A, Complement),
            graphStore::argumentFor(Complement, B).
        """.trimIndent()
}
