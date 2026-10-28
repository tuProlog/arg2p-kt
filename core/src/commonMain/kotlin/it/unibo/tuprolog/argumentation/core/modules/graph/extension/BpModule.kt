package it.unibo.tuprolog.argumentation.core.modules.graph.extension

import it.unibo.tuprolog.argumentation.core.modules.PrologModule

/**
 * The burdens of proof as part of the graph (module `bp`, a graphExtension): for an argument for `bp(Statement)` and
 * an argument for Statement, the burdened one, an argument for `-burdmet(Conclusion)` (top rule `artificial`) is
 * attacked as the burden of proof is, and rebuts or is rebutted by the burdened argument, depending on whether the
 * grounded labelling labels it IN.
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
            createBpArguments(BurdenedPairs, Pairs),
            utils::deduplicate(Pairs, BpPairs),
            createBpAttacks(BpPairs).

        % The conflict argument of a burdened argument is the same for every burden of proof on it: it is saved once, and
        % supported by each of them, so attacked as they are (see the module graphStore)
        createBpArguments([], []).
        createBpArguments([(Bp, Burdened)|Others], [(Conflict, Burdened)|BpPairs]) :-
            argument::rules(Burdened, Rules),
            argument::conclusion(Burdened, Conclusion),
            argument::infoParts(Info, [artificial], [artificial], []),
            argument::new([artificial|Rules], artificial, [-burdmet(Conclusion)], [], Info, Conflict),
            ignore(graphStore::saveArgument(a, [-burdmet(Conclusion)], Conflict)),
            ignore(graphStore::saveSupport(Bp, Conflict)),
            createBpArguments(Others, BpPairs).

        ignore(Goal) :- call(Goal), !.
        ignore(_).

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
            bp_grounded::argumentChain(BurdenedL, Burdened),
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
