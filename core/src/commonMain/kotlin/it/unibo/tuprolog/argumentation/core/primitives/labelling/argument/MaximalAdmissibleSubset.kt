package it.unibo.tuprolog.argumentation.core.primitives.labelling.argument

import it.unibo.tuprolog.argumentation.core.primitives.graph.attackGraph
import it.unibo.tuprolog.argumentation.core.primitives.ids
import it.unibo.tuprolog.argumentation.core.primitives.replyList
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.primitive.Primitive
import it.unibo.tuprolog.solve.primitive.Solve

/**
 * maximalAdmissibleSubset(+Ids, -Subset): the largest admissible subset of the conflict-free set Ids, removing the
 * arguments with an attacker not attacked by the remaining ones until none is left (a greatest fixpoint, polynomial)
 */
internal object MaximalAdmissibleSubset : Primitive {
    val signature: Signature = Signature("maximalAdmissibleSubset", 2)

    override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
        val attackers = request.attackGraph().attackers
        var subset = request.ids(0)
        while (true) {
            val defended =
                subset.filter { h ->
                    attackers[h].orEmpty().all { attacker ->
                        attackers[attacker].orEmpty().any { defendant -> defendant in subset && defendant != attacker }
                    }
                }
            if (defended.size == subset.size) break
            subset = defended
        }
        return sequenceOf(request.replyList(1, subset) ?: request.replyFail())
    }
}
