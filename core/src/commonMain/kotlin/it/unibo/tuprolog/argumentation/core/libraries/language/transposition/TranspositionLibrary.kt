package it.unibo.tuprolog.argumentation.core.libraries.language.transposition

import it.unibo.tuprolog.argumentation.core.libraries.ArgsFlag
import it.unibo.tuprolog.argumentation.core.libraries.PrologLibrary
import it.unibo.tuprolog.argumentation.core.libraries.language.transposition.flags.AutoTransposition
import it.unibo.tuprolog.argumentation.core.libraries.language.transposition.prolog.THEORY

/**
 * The transpositions of the strict rules (module `transposition`), with autoTransposition: for a strict rule
 * `r : a, b -> c`, the rules concluding a contradictory of each premise from a contradictory of the conclusion and
 * the other premises (`r_i : -c, b -> -a`, `r_ii : -c, a -> -b`; `transposition(r, '_i')` when a rule or premise
 * of the theory is named `r_i`, as a transposition never replaces one). The contradictories of a statement are its
 * negation, and the statements the theory declares in conflict with it both ways, with no guard
 * (`conflict([c], [d])` and `conflict([d], [c])`, the declared conflicts being part of the conflict model): the
 * transpositions through those are named transposed(Id, [ConclusionContradictory, PremiseContradictory]), Id the
 * name of the one through negations.
 */
object TranspositionLibrary : PrologLibrary("transposition") {
    override val baseFlags: Iterable<ArgsFlag>
        get() = listOf(AutoTransposition)

    override val parsingFlag: ArgsFlag
        get() = AutoTransposition

    override val prologRawTheory: String = THEORY
}
