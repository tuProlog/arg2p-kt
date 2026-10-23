package it.unibo.tuprolog.argumentation.core.primitives.labelling.argument

import it.unibo.tuprolog.argumentation.core.primitives.graph.attackGraph
import it.unibo.tuprolog.argumentation.core.primitives.graph.labelledIds
import it.unibo.tuprolog.argumentation.core.primitives.ids
import it.unibo.tuprolog.argumentation.core.primitives.replyList
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.primitive.Solve

/**
 * undecidedCandidates(+Scc, +Branch, -Ids): the UND arguments of Scc in Branch that can be forced IN, those not
 * attacked by an UND argument of a previous component (nothing left could defend them)
 */
internal object UndecidedCandidates : SccPrimitive {
    override val signature = Signature("undecidedCandidates", 3)

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
