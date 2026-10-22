package it.unibo.tuprolog.argumentation.core.primitives.labelling

import it.unibo.tuprolog.argumentation.core.primitives.graph.attackGraph
import it.unibo.tuprolog.argumentation.core.primitives.graph.labelledIds
import it.unibo.tuprolog.argumentation.core.primitives.ids
import it.unibo.tuprolog.argumentation.core.primitives.replyList
import it.unibo.tuprolog.core.Atom
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.primitive.Primitive
import it.unibo.tuprolog.solve.primitive.Solve

internal interface SccPrimitive : Primitive {
    val signature: Signature
}

/**
 * undecided_candidates(+Scc, +Branch, -Ids): the UND arguments of Scc in Branch that can be forced IN, those not
 * attacked by an UND argument of a previous component (nothing left could defend them)
 */
internal object UndecidedCandidates : SccPrimitive {
    override val signature = Signature("undecided_candidates", 3)

    override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
        val scc = request.ids(0)
        val undecided = request.labelledIds("und", request.arguments[1])
        val attackers = request.attackGraph().attackers
        val candidates =
            scc.filter { id ->
                id in undecided && attackers[id].orEmpty().none { it !in scc && it in undecided }
            }
        return sequenceOf(request.replyList(2, candidates) ?: request.replyFail())
    }
}

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
