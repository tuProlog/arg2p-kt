package it.unibo.tuprolog.argumentation.core.libraries.framework.utils

import it.unibo.tuprolog.argumentation.core.libraries.PrologLibrary
import it.unibo.tuprolog.argumentation.core.libraries.framework.utils.primitives.Contains
import it.unibo.tuprolog.argumentation.core.libraries.framework.utils.primitives.Deduplicate
import it.unibo.tuprolog.argumentation.core.libraries.framework.utils.primitives.Intersection
import it.unibo.tuprolog.argumentation.core.libraries.framework.utils.primitives.MinimalBySet
import it.unibo.tuprolog.argumentation.core.libraries.framework.utils.primitives.Sort
import it.unibo.tuprolog.argumentation.core.libraries.framework.utils.primitives.SortDistinct
import it.unibo.tuprolog.argumentation.core.libraries.framework.utils.primitives.Subsumes
import it.unibo.tuprolog.argumentation.core.libraries.framework.utils.primitives.Subtract
import it.unibo.tuprolog.argumentation.core.libraries.framework.utils.primitives.TermVariables
import it.unibo.tuprolog.argumentation.core.libraries.framework.utils.primitives.UniqueByFirst
import it.unibo.tuprolog.argumentation.core.libraries.framework.utils.prolog.THEORY
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.primitive.Primitive

/**
 * Utilities on lists and terms (module `utils`).
 */
object UtilsLibrary : PrologLibrary("utils") {
    override val primitives: Map<Signature, Primitive>
        get() =
            mapOf(
                Sort.signature to Sort,
                Deduplicate.signature to Deduplicate,
                SortDistinct.signature to SortDistinct,
                MinimalBySet.signature to MinimalBySet,
                UniqueByFirst.signature to UniqueByFirst,
                Intersection.signature to Intersection,
                Contains.signature to Contains,
                Subtract.signature to Subtract,
                Subsumes.signature to Subsumes,
                TermVariables.signature to TermVariables,
            )

    override val prologRawTheory: String = THEORY
}
