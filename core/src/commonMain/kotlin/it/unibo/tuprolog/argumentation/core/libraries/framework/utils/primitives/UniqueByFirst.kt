package it.unibo.tuprolog.argumentation.core.libraries.framework.utils.primitives

import it.unibo.tuprolog.argumentation.engine.natives.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.engine.natives.NativeCall
import it.unibo.tuprolog.argumentation.engine.natives.listArg
import it.unibo.tuprolog.argumentation.engine.natives.replyItems
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.exception.error.TypeError

/**
 * uniqueByFirst(+Lists, ?Unique): Lists keeping, among the lists with the same first element, only the last one
 * (in their order)
 */
internal object UniqueByFirst : AbstractNativePrimitive(Signature("uniqueByFirst", 2)) {
    override fun call(call: NativeCall): Int {
        val lists = call.listArg(0, signature)
        val seen = HashSet<Term>()
        val kept = BooleanArray(lists.size)
        for (i in lists.indices.reversed()) {
            val list = lists[i]
            if (call.kind(list) !=
                NativeCall.LIST
            ) {
                throw TypeError.forGoal(call.context, signature, TypeError.Expected.LIST, call.term(list))
            }
            kept[i] = seen.add(call.term(call.argOf(list, 0)))
        }
        return call.replyItems(1, lists.filterIndexed { i, _ -> kept[i] }.toIntArray())
    }
}
