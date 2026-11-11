package it.unibo.tuprolog.argumentation.core.libraries.graph.structured.extension.conflictArguments

import it.unibo.tuprolog.argumentation.core.libraries.ArgsFlag
import it.unibo.tuprolog.argumentation.core.libraries.PrologLibrary
import it.unibo.tuprolog.argumentation.core.libraries.graph.structured.extension.conflictArguments.prolog.THEORY
import it.unibo.tuprolog.argumentation.core.libraries.language.conflict.flags.MetaConflicts

/**
 * The arguments attacking through a conflict argument (module `conflictArguments`), with metaConflicts: conflicts
 * are then conclusions of the theory, and an argument attacks through the conflicts its conclusion has with others
 * only while their arguments stand.
 *
 * For each argument for a conflict on the conclusion of an argument, a copy of that argument is made, with
 * `mc(ConflictId, Conflict)` first among its rules: supported by the conflict argument (and by the supports of the
 * original), it is defeated with it, and it attacks through its conflict only (see the module attackRelation). With
 * metaConflicts only the copies attack.
 *
 * The step applies with metaConflicts (the flag of the module conflict), which makes it an extension of the theory (see
 * parser::extension/1).
 */
object ConflictArgumentsLibrary : PrologLibrary("conflictArguments") {
    override val extensionFlag: ArgsFlag
        get() = MetaConflicts

    override val prologRawTheory: String = THEORY
}
