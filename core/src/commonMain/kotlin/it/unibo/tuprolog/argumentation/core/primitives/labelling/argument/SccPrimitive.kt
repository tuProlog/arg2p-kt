package it.unibo.tuprolog.argumentation.core.primitives.labelling.argument

import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.primitive.Primitive

internal interface SccPrimitive : Primitive {
    val signature: Signature
}
