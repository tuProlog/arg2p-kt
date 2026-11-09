package it.unibo.tuprolog.argumentation.core.libraries.support.utils

import it.unibo.tuprolog.argumentation.core.libraries.PrologLibrary
import it.unibo.tuprolog.argumentation.core.libraries.support.utils.primitives.Contains
import it.unibo.tuprolog.argumentation.core.libraries.support.utils.primitives.Deduplicate
import it.unibo.tuprolog.argumentation.core.libraries.support.utils.primitives.Intersection
import it.unibo.tuprolog.argumentation.core.libraries.support.utils.primitives.MinimalBySet
import it.unibo.tuprolog.argumentation.core.libraries.support.utils.primitives.Sort
import it.unibo.tuprolog.argumentation.core.libraries.support.utils.primitives.SortDistinct
import it.unibo.tuprolog.argumentation.core.libraries.support.utils.primitives.Subsumes
import it.unibo.tuprolog.argumentation.core.libraries.support.utils.primitives.Subtract
import it.unibo.tuprolog.argumentation.core.libraries.support.utils.primitives.UniqueByFirst
import it.unibo.tuprolog.argumentation.core.libraries.support.utils.prolog.THEORY
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
            )

    override val prologRawTheory: String = THEORY
}
