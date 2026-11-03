package it.unibo.tuprolog.argumentation.core.primitives.graph.language

import it.unibo.tuprolog.argumentation.core.engine.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.core.engine.NativeCall
import it.unibo.tuprolog.argumentation.core.engine.listArg
import it.unibo.tuprolog.argumentation.core.engine.replyItems
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.exception.error.TypeError

/**
 * extractId(+Rules, -Ids): `strict(Id)` for the identifier of each of Rules (lists starting with it)
 */
internal object ExtractId : AbstractNativePrimitive(Signature("extractId", 2)) {
    override fun call(call: NativeCall): Int {
        val ids =
            call.listArg(0, signature).map { rule ->
                if (call.kind(rule) !=
                    NativeCall.LIST
                ) {
                    throw TypeError.forGoal(call.context, signature, TypeError.Expected.LIST, call.term(rule))
                }
                call.struct("strict", intArrayOf(call.argOf(rule, 0)))
            }
        return call.replyItems(1, ids.toIntArray())
    }
}
