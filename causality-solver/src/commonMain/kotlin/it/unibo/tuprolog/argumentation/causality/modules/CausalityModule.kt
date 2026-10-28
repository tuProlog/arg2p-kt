package it.unibo.tuprolog.argumentation.causality.modules

import it.unibo.tuprolog.argumentation.causality.primitives.ButFor
import it.unibo.tuprolog.argumentation.causality.primitives.Ness
import it.unibo.tuprolog.argumentation.causality.primitives.NessIntervention
import it.unibo.tuprolog.argumentation.causality.primitives.NessOriginal
import it.unibo.tuprolog.argumentation.causality.primitives.NessOriginalIntervention
import it.unibo.tuprolog.argumentation.causality.runtime.CausalityAnalysis
import it.unibo.tuprolog.argumentation.core.modules.PrologModule
import it.unibo.tuprolog.argumentation.core.modules.graph.language.ParserModule
import it.unibo.tuprolog.core.operators.OperatorSet
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.primitive.Primitive

/**
 * Whether a statement of a theory causes another (module `causality`): the NESS test, as published (Pisano, Prakken,
 * Sartor, Liepina; ICAIL 2025) and revised, and the but-for test, on the grounded labelling of the theory and of its
 * interventions. Unlike the modules of arg2p, each instance has a state of its own, the analysis computing the graphs
 * (and caching them): a solver is given a new one, [sandboxed] as the solver is (see Arg2pSolver.default).
 */
class CausalityModule(
    sandboxed: Boolean = false,
) : PrologModule("prolog.argumentation.causality.solver", "causality") {
    private val analysis = CausalityAnalysis(sandboxed)

    override val primitives: Map<Signature, Primitive>
        get() =
            listOf(Ness(analysis), NessIntervention(analysis), NessOriginal(analysis), NessOriginalIntervention(analysis), ButFor(analysis))
                .associateBy { it.signature }

    override val theoryOperators: OperatorSet
        get() = super.theoryOperators.plus(ParserModule.operators())

    override val prologRawTheory: String =
        // language=Prolog
        """
        %! subsets(?List, ?Subset)
        %  Subset is each subset of List, its elements in their order.
        subsets([], []).
        subsets([H|T], [H|NT]) :- subsets(T, NT).
        subsets([_|T], NT) :- subsets(T, NT).

        %! properSubsets(+List, -Subset)
        %  Subset is each subset of List other than the empty one and List itself.
        properSubsets(L, S) :-
            subsets(L, S),
            S \= [],
            S \= L.

        %! generateInterventions(+Rules, +Cause, -Intervention)
        %  Intervention is each change of the theory removing Cause: its complement, the undercut of Cause (a rule,
        %  unless it is negated), or the complement with a set of the premises of the rules not among Rules (the top
        %  rules of the graph of the effect).
        generateInterventions(_, S, [CS]) :-
            compl(S, CS).
        generateInterventions(_, S, [undercut(S)]) :-
            S \= -_.
        generateInterventions(R, S, SUP) :-
            parser:::convertAllRules(_),
            compl(S, CS),
            unusedPremises(R, UP), !,
            subsets(UP, SUP),
            member(CS, SUP),
            SUP \= [].

        %! compl(?Statement, ?Complement)
        %  Complement is the complement of Statement (-X for X, X for -X).
        compl(-X, X) :- !.
        compl(X, -X).

        %! unusedPremises(+Rules, -Premises)
        %  Premises are those of the rules of the theory not among Rules.
        unusedPremises(R, UP) :-
            findall(P, (parser::rule(Id, P, _), \+ member(Id, R), ground(Id)), RR),
            utils::appendLists(RR, UP).
        """.trimIndent()
}
