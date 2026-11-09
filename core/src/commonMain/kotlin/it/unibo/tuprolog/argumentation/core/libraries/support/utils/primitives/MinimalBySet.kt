package it.unibo.tuprolog.argumentation.core.libraries.support.utils.primitives

import it.unibo.tuprolog.argumentation.engine.vm.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.engine.vm.NativeCall
import it.unibo.tuprolog.argumentation.engine.vm.replyTerms
import it.unibo.tuprolog.argumentation.engine.vm.termArg
import it.unibo.tuprolog.core.List
import it.unibo.tuprolog.solve.Signature

/**
 * minimalBySet(+Pairs, -Values): the values of the Set-Value pairs whose Set has no strict subset among the others
 * (elements compared as ground terms), in order: minimal_set/2 over a whole collection in one step
 */
internal object MinimalBySet : AbstractNativePrimitive(Signature("minimalBySet", 2)) {
    override fun call(call: NativeCall): Int {
        val first = call.termArg(0) as? List ?: return NativeCall.FAIL
        val pairs =
            first.toList().map { pair ->
                val struct = pair.castToStruct()
                ((struct[0] as? List)?.toList()?.toSet() ?: return NativeCall.FAIL) to struct[1]
            }
        val sets = pairs.map { it.first }.distinct()
        val minimal = pairs.filter { (set, _) -> sets.none { other -> other != set && set.containsAll(other) } }
        return call.replyTerms(1, minimal.map { it.second })
    }
}
