package it.unibo.tuprolog.argumentation.causality

import it.unibo.tuprolog.argumentation.causality.helpers.CausalityAnalysis
import it.unibo.tuprolog.argumentation.causality.primitives.ButFor
import it.unibo.tuprolog.argumentation.causality.primitives.Ness
import it.unibo.tuprolog.argumentation.causality.primitives.NessIntervention
import it.unibo.tuprolog.argumentation.causality.prolog.THEORY
import it.unibo.tuprolog.argumentation.core.libraries.PrologLibrary
import it.unibo.tuprolog.argumentation.core.libraries.language.ruleNotation.notation.RuleNotation
import it.unibo.tuprolog.core.operators.OperatorSet
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.primitive.Primitive

/**
 * Whether a statement of a theory causes another (module `causality`): the NESS test, original (Pisano, Prakken,
 * Sartor, Liepina; ICAIL 2025: a subset-minimal intervention removes an explanation of the effect from the grounded
 * extension) and revised (ICAIL 2026: an argument for the cause is IN, and the intervention gives an argument IN or UND
 * attacking a member of the explanation that no proper part of it gives), and the but-for test, on the grounded labelling
 * of the theory and of its interventions. Unlike the modules of arg2p, each instance has a state of its own, the analysis computing the graphs
 * (and caching them): a solver is given a new one. The solver of the analysis is built at the first test, and prints
 * where the solver testing does.
 */
class CausalityLibrary : PrologLibrary("causality") {
    private val analysis = CausalityAnalysis()

    override val primitives: Map<Signature, Primitive>
        get() =
            listOf(
                Ness(analysis, revised = true),
                Ness(analysis, revised = false),
                NessIntervention(analysis, revised = true),
                NessIntervention(analysis, revised = false),
                ButFor(analysis),
            ).associateBy { it.signature }

    override val theoryOperators: OperatorSet
        get() = RuleNotation.theories

    override val prologRawTheory: String = THEORY
}
