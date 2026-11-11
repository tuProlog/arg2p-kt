package it.unibo.tuprolog.argumentation.core.libraries.labelling.argument.naive

import it.unibo.tuprolog.argumentation.core.libraries.PrologLibrary
import it.unibo.tuprolog.argumentation.core.libraries.labelling.argument.naive.primitives.NaiveSubset
import it.unibo.tuprolog.argumentation.core.libraries.labelling.argument.naive.prolog.THEORY
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.primitive.Primitive

/**
 * The naive labellings (module `naive`, an argumentLabellingMode): the conflict-free ones with a maximal set of IN
 * arguments.
 */
object NaiveLibrary : PrologLibrary("naive") {
    override val primitives: Map<Signature, Primitive>
        get() = mapOf(NaiveSubset.signature to NaiveSubset)

    override val prologRawTheory: String = THEORY
}
