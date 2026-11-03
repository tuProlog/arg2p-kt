package it.unibo.tuprolog.argumentation.core.primitives.labelling.argument

import it.unibo.tuprolog.argumentation.core.engine.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.core.engine.NativeCall
import it.unibo.tuprolog.argumentation.core.engine.atomArg
import it.unibo.tuprolog.argumentation.core.engine.replyTerms
import it.unibo.tuprolog.argumentation.core.engine.termsArg
import it.unibo.tuprolog.argumentation.core.primitives.graph.labelledIds
import it.unibo.tuprolog.core.Atom
import it.unibo.tuprolog.solve.Signature

/**
 * selection(+Select, +Scc, +Explored, -Selected): the branches of the local labellings in Explored (a list of
 * Branch-Ok) kept by Select: all the accepted ones, or the accepted ones whose IN arguments in Scc are maximal
 */
internal object Selection : AbstractNativePrimitive(Signature("selection", 4)) {
    override fun call(call: NativeCall): Int {
        val accepted =
            call
                .termsArg(2, signature)
                .map { it.castToStruct() }
                .filter { (it[1] as? Atom)?.value == "true" }
                .map { it[0] }
        val selected =
            when (call.atomArg(0, signature)) {
                "maximal" -> {
                    val scc = call.termsArg(1, signature).toSet()
                    val local = accepted.associateWith { call.labelledIds("in", it).intersect(scc) }
                    accepted.filter { state ->
                        local.values.none { other -> other != local[state] && other.containsAll(local.getValue(state)) }
                    }
                }
                else -> accepted
            }
        return call.replyTerms(3, selected)
    }
}
