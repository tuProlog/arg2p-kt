package it.unibo.tuprolog.argumentation.core.libraries.framework.context.primitives

import it.unibo.tuprolog.argumentation.core.libraries.framework.context.helpers.ContextBranches
import it.unibo.tuprolog.argumentation.engine.natives.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.engine.natives.NativeCall
import it.unibo.tuprolog.argumentation.engine.natives.replyWord
import it.unibo.tuprolog.solve.Signature

/**
 * contextCount(+Goal, +Limit, -Count): the number of solutions of Goal in the active branch, counted up to Limit
 * (an integer, or anything else for no limit): enough to compare how selective goals are, without visiting all
 * the solutions of the least selective ones
 */
internal class ContextCount(
    private val branches: ContextBranches,
) : AbstractNativePrimitive(Signature("contextCount", 3)) {
    override fun call(call: NativeCall): Int {
        val limitWord = call.arg(1)
        val limit = if (call.kind(limitWord) == NativeCall.INT) call.intValue(limitWord).toInt() else Int.MAX_VALUE
        val count = call.countSolutions(call.struct("contextCheck", intArrayOf(call.arg(0))), limit)
        return call.replyWord(2, call.int(count))
    }
}
