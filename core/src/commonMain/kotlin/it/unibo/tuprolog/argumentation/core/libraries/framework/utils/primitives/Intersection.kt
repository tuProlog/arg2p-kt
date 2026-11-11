package it.unibo.tuprolog.argumentation.core.libraries.framework.utils.primitives

import it.unibo.tuprolog.argumentation.engine.natives.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.engine.natives.NativeCall
import it.unibo.tuprolog.argumentation.engine.natives.replyTerms
import it.unibo.tuprolog.argumentation.engine.natives.termArg
import it.unibo.tuprolog.core.List
import it.unibo.tuprolog.solve.Signature

/**
 * intersectionOf(+Sets, -Intersection): the elements of the first list in Sets that are in all of them, in its order
 * (elements compared as ground terms); fails if Sets is empty
 */
internal object Intersection : AbstractNativePrimitive(Signature("intersectionOf", 2)) {
    override fun call(call: NativeCall): Int {
        val first = call.termArg(0) as? List ?: return NativeCall.FAIL
        val sets = first.toList().map { (it as? List)?.toList() ?: return NativeCall.FAIL }
        if (sets.isEmpty()) return NativeCall.FAIL
        val others = sets.drop(1).map { it.toSet() }
        return call.replyTerms(1, sets.first().filter { y -> others.all { y in it } })
    }
}
