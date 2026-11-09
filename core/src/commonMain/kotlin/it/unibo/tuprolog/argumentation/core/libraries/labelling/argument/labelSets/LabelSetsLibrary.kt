package it.unibo.tuprolog.argumentation.core.libraries.labelling.argument.labelSets

import it.unibo.tuprolog.argumentation.core.libraries.PrologLibrary
import it.unibo.tuprolog.argumentation.core.libraries.labelling.argument.labelSets.primitives.AdmissibleWithin
import it.unibo.tuprolog.argumentation.core.libraries.labelling.argument.labelSets.primitives.MaximalAdmissibleSubset
import it.unibo.tuprolog.argumentation.core.libraries.labelling.argument.labelSets.primitives.NewLabelling
import it.unibo.tuprolog.argumentation.core.libraries.labelling.argument.labelSets.prolog.THEORY
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.primitive.Primitive

/**
 * What the semantics share (module `labelSets`, no semantics itself): the arguments to label, the labelling of a set of
 * arguments IN, the labellings of a semantics refined into those of another, and whether a component is defended.
 */
object LabelSetsLibrary : PrologLibrary("labelSets") {
    override val primitives: Map<Signature, Primitive>
        get() = listOf(NewLabelling, AdmissibleWithin, MaximalAdmissibleSubset).associateBy { it.signature }

    override val prologRawTheory: String = THEORY
}
