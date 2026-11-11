package it.unibo.tuprolog.argumentation.core.libraries.labelling.argument.stronglyAdmissible

import it.unibo.tuprolog.argumentation.core.libraries.PrologLibrary
import it.unibo.tuprolog.argumentation.core.libraries.labelling.argument.stronglyAdmissible.primitives.StronglyAdmissibleSet
import it.unibo.tuprolog.argumentation.core.libraries.labelling.argument.stronglyAdmissible.prolog.THEORY
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.primitive.Primitive

/**
 * The strongly admissible labellings (module `stronglyAdmissible`, an argumentLabellingMode): the admissible ones
 * whose IN arguments are defended, step by step, starting from the unattacked ones.
 */
object StronglyAdmissibleLibrary : PrologLibrary("stronglyAdmissible") {
    override val primitives: Map<Signature, Primitive>
        get() = mapOf(StronglyAdmissibleSet.signature to StronglyAdmissibleSet)

    override val prologRawTheory: String = THEORY
}
