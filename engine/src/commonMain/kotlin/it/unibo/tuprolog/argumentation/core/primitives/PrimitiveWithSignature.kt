package it.unibo.tuprolog.argumentation.core.primitives

import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.primitive.Primitive

/** A primitive that knows the predicate it implements */
interface PrimitiveWithSignature : Primitive {
    val signature: Signature
}
