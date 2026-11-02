package it.unibo.tuprolog.argumentation.core.modules.graph.extension

import it.unibo.tuprolog.argumentation.core.modules.PrologModule

/**
 * The burdens of proof as part of the graph (module `bp`, a graphExtension): for an argument for `bp(Statement)` and
 * an argument for Statement, the burdened one, an argument for `-burdmet(Conclusion)` (top rule `artificial`, one for
 * each of the two) is attacked as the burden of proof is, and rebuts or is rebutted by the burdened argument,
 * depending on whether the grounded labelling labels it IN.
 */
object BpModule : PrologModule("prolog.argumentation.graph.meta.bp", "bp") {
    override val prologRawTheory: String =
        // language=Prolog
        """
        %! modifyArgumentationGraph
        %  The arguments for the burdens met, and their attacks with the burdened arguments.
        modifyArgumentationGraph :-
            findall((Bp, Burdened), (
                argument::claim(Bp, bp(Statement)),
                graphStore::argument(Bp),
                argument::claim(Burdened, Statement),
                graphStore::argument(Burdened)
            ), BurdenedPairs),
            checkBpArguments(BurdenedPairs).

        checkBpArguments([]) :- !.
        checkBpArguments(BurdenedPairs) :-
            createBpArguments(BurdenedPairs, BpPairs),
            createBpAttacks(BpPairs).

        % The conflict argument of a burdened argument for each argument for a burden of proof on it (its rules those
        % of both, after artificial): supported by that burden argument, so attacked as it is (see the module
        % graphStore), the other burden arguments standing on their own
        createBpArguments([], []).
        createBpArguments([(Bp, Burdened)|Others], [(Conflict, Burdened)|BpPairs]) :-
            argument::rules(Bp, BpRules),
            argument::rules(Burdened, Rules),
            utils::appendFast(BpRules, Rules, AllRules),
            argument::conclusion(Burdened, Conclusion),
            argument::infoParts(Info, [artificial], [artificial], []),
            argument::new([artificial|AllRules], artificial, [-burdmet(Conclusion)], [], Info, Conflict),
            graphStore::saveArgument([-burdmet(Conclusion)], Conflict),
            graphStore::saveSupport(Bp, Conflict),
            createBpArguments(Others, BpPairs).

        createBpAttacks(BpPairs) :-
            generateBpsEvaluationChain(BpPairs, OrderedBpPairs),
            evaluateBurdenedArgs(OrderedBpPairs).

        % An evaluation order for the pairs, based on the attacks between them: if a node A is reached from a node B
        % through attacks, B is evaluated before A. Cycles are not admitted.
        generateBpsEvaluationChain([], []).
        generateBpsEvaluationChain([BpPair|BpPairs], OrderedBpPairs) :-
            generateBpsEvaluationChain(BpPairs, TempOrderedBpPairs),
            insertBpPair(BpPair, TempOrderedBpPairs, OrderedBpPairs).

        insertBpPair(BpPair, [], [BpPair]).
        insertBpPair((Bp, Burdened), [(BpL, BurdenedL)|Others], [(BpL, BurdenedL)|Return]) :-
            graphStore::reaches(BurdenedL, Burdened),
            insertBpPair((Bp, Burdened), Others, Return), !.
        insertBpPair((Bp, Burdened), [(BpL, BurdenedL)|Others], [(Bp, Burdened)|[(BpL, BurdenedL)|Others]]).

        % Each pair in turn: the burdened argument rebuts the conflict argument if the grounded labelling labels it IN,
        % otherwise it is rebutted
        evaluateBurdenedArgs([]).
        evaluateBurdenedArgs([(Bp, Burdened)|Others]) :-
            context_active(X),
            grounded:::argumentLabelling,
            statusToAttack((Bp, Burdened), attack(T, A, B, C)),
            context_checkout(X),
            graphStore::saveAttack(T, A, B, C),
            evaluateBurdenedArgs(Others).

        statusToAttack((Bp, Burdened), attack(bprebut, Burdened, Bp, Bp)) :-
            graphStore::argumentLabel(in, Burdened).
        statusToAttack((Bp, Burdened), attack(bprebut, Bp, Burdened, Burdened)) :-
            \+ graphStore::argumentLabel(in, Burdened).

        %==============================================================================
        % QUERY MODE
        %==============================================================================

        %! queryDependencies(+Argument, -Statement)
        %  A burden of proof on a statement is itself a conclusion (bp/1), related to the arguments for the statement.
        queryDependencies(Argument, bp(Claim)) :-
            argument::claim(Argument, Claim).

        %! queryDependants(+Argument, -Statement)
        %  The arguments for a statement depend on the burden of proof on it.
        queryDependants(Argument, Claim) :-
            argument::claim(Argument, bp(Claim)).
        """.trimIndent()
}
