package it.unibo.tuprolog.argumentation.core.primitives.labelling

import it.unibo.tuprolog.argumentation.core.primitives.graph.labelledIds
import it.unibo.tuprolog.argumentation.core.primitives.ids
import it.unibo.tuprolog.argumentation.core.primitives.replyList
import it.unibo.tuprolog.core.Atom
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.primitive.Solve

/**
 * selection(+Select, +Scc, +Explored, -Selected): the branches of the local labellings in Explored (a list of
 * Branch-Ok) kept by Select: all the accepted ones, or the accepted ones whose IN arguments in Scc are maximal
 */
internal object Selection : SccPrimitive {
    override val signature = Signature("selection", 4)

    override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
        val accepted =
            request
                .ids(2)
                .map { it.castToStruct() }
                .filter { (it[1] as? Atom)?.value == "true" }
                .map { it[0] }
        val selected =
            when (request.arguments[0].castToAtom().value) {
                "maximal" -> {
                    val scc = request.ids(1).toSet()
                    val local = accepted.associateWith { request.labelledIds("in", it).intersect(scc) }
                    accepted.filter { state ->
                        local.values.none { other -> other != local[state] && other.containsAll(local.getValue(state)) }
                    }
                }
                else -> accepted
            }
        return sequenceOf(request.replyList(3, selected) ?: request.replyFail())
    }
}
