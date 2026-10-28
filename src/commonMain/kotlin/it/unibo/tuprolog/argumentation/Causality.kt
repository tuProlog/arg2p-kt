package it.unibo.tuprolog.argumentation

import it.unibo.tuprolog.argumentation.core.model.terms.TermValues
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Var
import kotlin.js.JsExport

/**
 * Whether a statement of the theory of a solver causes another (module `causality`), on the grounded labelling of the
 * theory and of its interventions: the NESS test, as published (Pisano, Prakken, Sartor, Liepina; ICAIL 2025) and
 * revised, and the but-for test. The statements are Prolog text; an intervention is a list of statements, those
 * changing the theory to remove the cause.
 */
@JsExport
class Causality internal constructor(
    private val arg2p: Arg2p,
) {
    /**
     * Whether [cause] is a NESS cause of [effect] (the revised condition): an intervention removing it refutes an
     * explanation of the effect as no part of it does; the intervention, null if it is not a cause
     */
    fun ness(
        cause: String,
        effect: String,
    ): Value? = intervention("ness", cause, effect)

    /** Whether [cause] is a NESS cause of [effect] as published: the intervention, null if it is not a cause */
    fun nessOriginal(
        cause: String,
        effect: String,
    ): Value? = intervention("ness_original", cause, effect)

    /** Whether [intervention], a list of statements, meets the revised NESS condition for [effect] */
    fun nessIntervention(
        intervention: String,
        effect: String,
    ): Boolean = holds("ness_intervention", intervention, effect)

    /** Whether [intervention], a list of statements, meets the NESS condition for [effect] as published */
    fun nessOriginalIntervention(
        intervention: String,
        effect: String,
    ): Boolean = holds("ness_original_intervention", intervention, effect)

    /**
     * Whether [cause] is a but-for cause of [effect]: the effect is accepted, and it is not once the complement of the
     * cause is added to the theory
     */
    fun butFor(
        cause: String,
        effect: String,
    ): Boolean = holds("but_for", cause, effect)

    private fun intervention(
        test: String,
        cause: String,
        effect: String,
    ): Value? {
        val intervention = Var.of("Intervention")
        return arg2p.first(call(test, intervention, arg2p.term(cause), arg2p.term(effect)))?.substitution?.get(intervention)?.let {
            TermValues.of(it)
        }
    }

    private fun holds(
        test: String,
        vararg arguments: String,
    ): Boolean = arg2p.first(call(test, *arguments.map { arg2p.term(it) }.toTypedArray())) != null

    // The test, on a context reset: each test is independent of those before it
    private fun call(
        test: String,
        vararg arguments: it.unibo.tuprolog.core.Term,
    ) = Struct.of(",", Struct.of("context_reset"), Struct.of("::", Struct.of("causality"), Struct.of(test, arguments.toList())))
}
