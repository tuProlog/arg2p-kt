package it.unibo.tuprolog.argumentation.core.libs.graph.labelling

import it.unibo.tuprolog.argumentation.core.libs.PrologModule
import it.unibo.tuprolog.argumentation.core.libs.graph.attackGraph
import it.unibo.tuprolog.argumentation.core.libs.graph.labelled
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.primitive.Primitive
import it.unibo.tuprolog.solve.primitive.Solve

object StronglyAdmissibleLabeller : PrologModule("prolog.argumentation.graph.labelling.stronglyadmissible", "stronglyadmissible") {
    override val primitives: Map<Signature, Primitive>
        get() = mapOf(StronglyAdmissibleSet.signature to StronglyAdmissibleSet)

    override val prologRawTheory: String =
        """
        % Query mode: the label of an argument only depends on the arguments it depends on (directionality)
        locality(directional).

        argumentLabelling :-
            admissible::argumentLabelling,
            strongly_admissible_set.
        """.trimIndent()
}

/**
 * strongly_admissible_set: every IN argument is reachable from the unattacked ones, iteratively adding the IN
 * arguments whose attackers are all attacked by those added so far
 */
object StronglyAdmissibleSet : Primitive {
    val signature: Signature = Signature("strongly_admissible_set", 0)

    override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
        val attackers = request.attackGraph().attackers
        val built = mutableSetOf<Term>()
        var pending = request.labelled("inId")
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
