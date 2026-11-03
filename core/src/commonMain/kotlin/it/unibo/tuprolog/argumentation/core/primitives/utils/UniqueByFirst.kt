package it.unibo.tuprolog.argumentation.core.primitives.utils

import it.unibo.tuprolog.argumentation.core.engine.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.core.engine.NativeCall
import it.unibo.tuprolog.argumentation.core.engine.replyTerms
import it.unibo.tuprolog.argumentation.core.engine.termArg
import it.unibo.tuprolog.core.List
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.solve.Signature

/**
 * uniqueByFirst(+Lists, ?Unique): Lists keeping, among the lists with the same first element, only the last one
 * (in their order)
 */
internal object UniqueByFirst : AbstractNativePrimitive(Signature("uniqueByFirst", 2)) {
    override fun call(call: NativeCall): Int {
        val first = call.termArg(0) as? List ?: return NativeCall.FAIL
        val seen = HashSet<Term>()
        val unique =
            first
                .toList()
                .asReversed()
                .filter { seen.add(it.castToList().toList().first()) }
                .asReversed()
        return call.replyTerms(1, unique)
    }
}
