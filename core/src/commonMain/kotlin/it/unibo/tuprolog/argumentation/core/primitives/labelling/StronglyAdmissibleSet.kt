package it.unibo.tuprolog.argumentation.core.primitives.labelling

import it.unibo.tuprolog.argumentation.core.primitives.graph.attackGraph
import it.unibo.tuprolog.argumentation.core.primitives.graph.labelledIds
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.primitive.Primitive
import it.unibo.tuprolog.solve.primitive.Solve

/**
 * strongly_admissible_set: every IN argument is reachable from the unattacked ones, iteratively adding the IN
 * arguments whose attackers are all attacked by those added so far
 */
object StronglyAdmissibleSet : Primitive {
    val signature: Signature = Signature("strongly_admissible_set", 0)

    override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
        val attackers = request.attackGraph().attackers
        val built = mutableSetOf<Term>()
        var pending = request.labelledIds("in")
        while (pending.isNotEmpty()) {
            val added =
                pending.filter { h ->
                    attackers[h].orEmpty().all { attacker -> attackers[attacker].orEmpty().any { it in built } }
                }
            if (added.isEmpty()) return sequenceOf(request.replyFail())
            built += added
            pending = pending - added.toSet()
        }
        return sequenceOf(request.replyWith(true))
    }
}
