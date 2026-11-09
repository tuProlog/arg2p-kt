package it.unibo.tuprolog.argumentation.core.modules.labelling.argument

import it.unibo.tuprolog.argumentation.core.graph.idList
import it.unibo.tuprolog.argumentation.core.graph.idsArg
import it.unibo.tuprolog.argumentation.core.graph.labelledIds
import it.unibo.tuprolog.argumentation.engine.vm.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.engine.vm.NativeCall
import it.unibo.tuprolog.argumentation.engine.vm.atomArg
import it.unibo.tuprolog.argumentation.engine.vm.listArg
import it.unibo.tuprolog.argumentation.engine.vm.replyWord
import it.unibo.tuprolog.argumentation.engine.words.IdSet
import it.unibo.tuprolog.solve.Signature

/**
 * selection(+Select, +Scc, +Explored, -Selected): the branches of the local labellings in Explored (a list of
 * Branch-Ok) kept by Select: all the accepted ones, or the accepted ones whose IN arguments in Scc are maximal
 */
internal object Selection : AbstractNativePrimitive(Signature("selection", 4)) {
    override fun call(call: NativeCall): Int {
        val accepted =
            call
                .listArg(2, signature)
                .filter { call.kind(call.argOf(it, 1)) == NativeCall.ATOM && call.atomName(call.argOf(it, 1)) == "true" }
                .map { call.intValue(call.argOf(it, 0)).toInt() }
        val selected =
            if (call.atomArg(0, signature) == "maximal" && accepted.size > 1) {
                val scc = IdSet.of(call.idsArg(1, signature))
                val local = accepted.map { call.labelledIds("in", it).intersect(scc) }
                accepted.filterIndexed { i, _ ->
                    local.none { other -> !other.sameAs(local[i]) && other.containsAll(local[i]) }
                }
            } else {
                accepted
            }
        return call.replyWord(3, call.idList(selected.toIntArray()))
    }
}
