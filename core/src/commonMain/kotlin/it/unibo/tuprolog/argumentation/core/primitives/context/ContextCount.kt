package it.unibo.tuprolog.argumentation.core.primitives.context

import it.unibo.tuprolog.argumentation.core.engine.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.core.engine.NativeCall
import it.unibo.tuprolog.argumentation.core.engine.replyWord
import it.unibo.tuprolog.argumentation.core.runtime.context.ContextBranches
import it.unibo.tuprolog.solve.Signature

/**
 * context_count(+Goal, +Limit, -Count): the number of solutions of Goal in the active branch, counted up to Limit
 * (an integer, or anything else for no limit): enough to compare how selective goals are, without visiting all
 * the solutions of the least selective ones
 */
internal class ContextCount(
    private val branches: ContextBranches,
) : AbstractNativePrimitive(Signature("context_count", 3)) {
    override fun call(call: NativeCall): Int {
        val limitWord = call.arg(1)
        val limit = if (call.kind(limitWord) == NativeCall.INT) call.intValue(limitWord).toInt() else Int.MAX_VALUE
        val count = call.countSolutions(call.struct("context_check", intArrayOf(call.arg(0))), limit)
        return call.replyWord(2, call.int(count))
    }
}
