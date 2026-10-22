package it.unibo.tuprolog.argumentation.core.libs.graph.labelling

import it.unibo.tuprolog.argumentation.core.libs.PrologModule
import it.unibo.tuprolog.argumentation.core.libs.graph.attackGraph
import it.unibo.tuprolog.argumentation.core.libs.graph.ids
import it.unibo.tuprolog.argumentation.core.libs.graph.labelled
import it.unibo.tuprolog.argumentation.core.libs.graph.replyList
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.primitive.Primitive
import it.unibo.tuprolog.solve.primitive.Solve

object AdmissibleLabeller : PrologModule("prolog.argumentation.graph.labelling.admissible", "admissible") {
    override val primitives: Map<Signature, Primitive>
        get() =
            (listOf(AdmissibleSet(0), AdmissibleSet(1)).associateBy { it.signature }) +
                (MaximalAdmissibleSubset.signature to MaximalAdmissibleSubset)

    override val prologRawTheory: String =
        """
        % Query mode: the label of an argument only depends on the arguments it depends on (directionality)
        locality(directional).

        argumentLabelling :-
            context_active(Branch),
            conflictfree::candidates(Args, Forced),
            conflictfree::conflict_free_subset(Args, Forced, Set),
            admissible_set(Set),
            conflictfree::labelSet(Branch, Set).
        """.trimIndent()
}

/**
 * admissible_set: every attacker of an IN argument is attacked by an IN argument other than itself.
 * admissible_set(+Ids): the same, with the arguments in Ids also IN.
 */
class AdmissibleSet(
    arity: Int,
) : Primitive {
    val signature: Signature = Signature("admissible_set", arity)

    override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
        val inIds = request.labelled("inId") + if (request.arguments.isEmpty()) emptyList() else request.ids(0)
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

/**
 * maximal_admissible_subset(+Ids, -Subset): the largest admissible subset of the conflict-free set Ids, removing the
 * arguments with an attacker not attacked by the remaining ones until none is left (a greatest fixpoint, polynomial)
 */
object MaximalAdmissibleSubset : Primitive {
    val signature: Signature = Signature("maximal_admissible_subset", 2)

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
