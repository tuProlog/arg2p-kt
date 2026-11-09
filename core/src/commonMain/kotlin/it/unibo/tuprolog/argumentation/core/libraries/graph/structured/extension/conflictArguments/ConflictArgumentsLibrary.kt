package it.unibo.tuprolog.argumentation.core.libraries.graph.structured.extension.conflictArguments

import it.unibo.tuprolog.argumentation.core.libraries.PrologLibrary
import it.unibo.tuprolog.argumentation.core.libraries.graph.structured.extension.conflictArguments.prolog.THEORY

/**
 * The arguments attacking through a conflict argument (module `conflictArguments`), with metaConflicts: conflicts
 * are then conclusions of the theory, and an argument attacks through the conflicts its conclusion has with others
 * only while their arguments stand.
 *
 * For each argument for a conflict on the conclusion of an argument, a copy of that argument is made, with
 * `mc(ConflictId, Conflict)` first among its rules: supported by the conflict argument (and by the supports of the
 * original), it is defeated with it, and it attacks through its conflict only (see the module attackRelation). With
 * metaConflicts only the copies attack.
 */
object ConflictArgumentsLibrary : PrologLibrary("conflictArguments") {
    override val prologRawTheory: String = THEORY
}
