package it.unibo.tuprolog.argumentation.core.libraries.language.conflict

import it.unibo.tuprolog.argumentation.core.libraries.ArgsFlag
import it.unibo.tuprolog.argumentation.core.libraries.PrologLibrary
import it.unibo.tuprolog.argumentation.core.libraries.language.conflict.flags.ConflictModel
import it.unibo.tuprolog.argumentation.core.libraries.language.conflict.flags.MetaConflicts
import it.unibo.tuprolog.argumentation.core.libraries.language.conflict.prolog.THEORY

/**
 * The conflicts between statements (module `conflict`). Without metaConflicts they are those of the parts of the
 * conflict model stated (conflictModel, all of them by default): negation (a literal and its negation), deontic (the
 * obligations and permissions in conflict), preference (a superiority and its converse) and declared (those the
 * theory declares, conflict/2,3); with metaConflicts they are only those concluded by an argument for
 * `conflict(A, B)` or `conflict(A, B, Guard)`.
 *
 * The conflicts the languages of the theory state (conflicts/1, see the language part of the module) hold
 * whatever the parts: with none of them (`conflictModel(none)`), they are the only ones.
 *
 * Statements are written as in the conclusions of arguments, `[Claim]`. A conflict may hold only under a guard, a
 * Prolog goal (or a term true or false) checked when the conflict is used.
 */
object ConflictLibrary : PrologLibrary("conflict") {
    override val baseFlags: Iterable<ArgsFlag>
        get() = listOf(ConflictModel, MetaConflicts)

    override val prologRawTheory: String = THEORY
}
