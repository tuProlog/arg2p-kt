package it.unibo.tuprolog.argumentation.core.primitives.labelling.argument

import it.unibo.tuprolog.argumentation.core.primitives.graph.attackGraph
import it.unibo.tuprolog.argumentation.core.primitives.graph.labelledIds
import it.unibo.tuprolog.argumentation.core.primitives.ids
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.primitive.Primitive
import it.unibo.tuprolog.solve.primitive.Solve

/**
 * admissibleSet: every attacker of an IN argument is attacked by an IN argument other than itself.
 * admissibleSet(+Ids): every attacker of an IN argument, or of an argument in Ids, is attacked by one of them other
 * than itself (the arguments in Ids taken as IN).
 */
class AdmissibleSet(
    arity: Int,
) : Primitive {
    val signature: Signature = Signature("admissibleSet", arity)

    override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
        val inIds = request.labelledIds("in") + if (request.arguments.isEmpty()) emptyList() else request.ids(0)
        val attackers = request.attackGraph().attackers
        val admissible =
            inIds.all { h ->
                attackers[h].orEmpty().all { attacker ->
                    attackers[attacker].orEmpty().any { defendant -> defendant in inIds && defendant != attacker }
                }
            }
        return sequenceOf(request.replyWith(admissible))
    }
}
