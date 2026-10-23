package it.unibo.tuprolog.argumentation.core.primitives.labelling

import it.unibo.tuprolog.argumentation.core.dsl.arg2pScope
import it.unibo.tuprolog.argumentation.core.primitives.graph.attackGraph
import it.unibo.tuprolog.argumentation.core.primitives.graph.labelArguments
import it.unibo.tuprolog.argumentation.core.primitives.graph.labelledIds
import it.unibo.tuprolog.argumentation.core.primitives.graph.storedArguments
import it.unibo.tuprolog.argumentation.core.primitives.ids
import it.unibo.tuprolog.argumentation.core.runtime.context.contextBranch
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.primitive.Primitive
import it.unibo.tuprolog.solve.primitive.Solve

/**
 * label_set(+Branch, +Ids): in a new branch of Branch (left active), the arguments of Ids IN, the unlabelled
 * arguments attacked by an IN one OUT, and the other unlabelled ones UND, asserted with a single context_assert_all
 */
object LabelSet : Primitive {
    val signature: Signature = Signature("label_set", 2)

    override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> =
        arg2pScope {
            if (!request.contextBranch(request.arguments[0])) return@arg2pScope sequenceOf(request.replyFail())
            val set = request.ids(1).toSet()
            val arguments = request.storedArguments()
            val labelled = request.labelledIds("in") + request.labelledIds("out") + request.labelledIds("und")
            val newIn = arguments.filter { (id, _) -> id in set && id !in labelled }
            val inIds = request.labelledIds("in") + newIn.map { it.first }
            val attackers = request.attackGraph().attackers
            val (out, und) =
                arguments
                    .filter { (id, _) -> id !in labelled && id !in inIds }
                    .partition { (id, _) -> attackers[id].orEmpty().any { it in inIds } }
            request.labelArguments(listOf(Pair("in", newIn), Pair("out", out), Pair("und", und)))
            sequenceOf(request.replyWith(true))
        }
}
