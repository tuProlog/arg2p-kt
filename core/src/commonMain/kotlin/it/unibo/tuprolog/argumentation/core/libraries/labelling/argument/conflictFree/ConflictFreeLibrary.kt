package it.unibo.tuprolog.argumentation.core.libraries.labelling.argument.conflictFree

import it.unibo.tuprolog.argumentation.core.libraries.PrologLibrary
import it.unibo.tuprolog.argumentation.core.libraries.labelling.argument.conflictFree.primitives.ConflictFreeSubset
import it.unibo.tuprolog.argumentation.core.libraries.labelling.argument.conflictFree.prolog.THEORY
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.primitive.Primitive

/**
 * The conflict-free labellings (module `conflictFree`, an argumentLabellingMode): a set of arguments IN, none
 * attacking another one, those they attack OUT and the rest UND.
 */
object ConflictFreeLibrary : PrologLibrary("conflictFree") {
    override val primitives: Map<Signature, Primitive>
        get() = mapOf(ConflictFreeSubset.signature to ConflictFreeSubset)

    override val prologRawTheory: String = THEORY
}
