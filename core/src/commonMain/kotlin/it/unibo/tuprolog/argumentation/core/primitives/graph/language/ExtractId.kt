package it.unibo.tuprolog.argumentation.core.primitives.graph.language

import it.unibo.tuprolog.argumentation.core.engine.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.core.engine.NativeCall
import it.unibo.tuprolog.argumentation.core.engine.replyTerms
import it.unibo.tuprolog.argumentation.core.engine.termsArg
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.solve.Signature

/**
 * extractId(+Rules, -Ids): `strict(Id)` for the identifier of each of Rules (lists starting with it)
 */
internal object ExtractId : AbstractNativePrimitive(Signature("extractId", 2)) {
    override fun call(call: NativeCall): Int =
        call.replyTerms(1, call.termsArg(0, signature).map { Struct.of("strict", it.asCons()!!.head) })
}
