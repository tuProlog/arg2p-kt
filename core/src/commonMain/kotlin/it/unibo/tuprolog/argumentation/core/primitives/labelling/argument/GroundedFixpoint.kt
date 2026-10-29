package it.unibo.tuprolog.argumentation.core.primitives.labelling.argument

import it.unibo.tuprolog.argumentation.core.dsl.arg2pScope
import it.unibo.tuprolog.argumentation.core.primitives.graph.attackGraph
import it.unibo.tuprolog.argumentation.core.primitives.graph.labelArguments
import it.unibo.tuprolog.argumentation.core.primitives.graph.labelledIds
import it.unibo.tuprolog.argumentation.core.primitives.graph.storedArguments
import it.unibo.tuprolog.argumentation.core.primitives.ids
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.primitive.Primitive
import it.unibo.tuprolog.solve.primitive.Solve

/**
 * groundedFixpoint: extends the IN/OUT labelling of the active context to the grounded fixpoint, labelling UND
 * the arguments left. Each round labels IN the arguments whose attackers are all OUT, then OUT the arguments
 * attacked by an IN one; the context is read once and only the new labels are asserted.
 *
 * Each argument keeps the number of its attackers not OUT: a round only looks at the arguments the previous one
 * changed (those attacked by the arguments just labelled), rather than at all the unlabelled ones, which made a
 * chain of attacks quadratic. The labels of a round are asserted in the order of the arguments.
 */
internal object GroundedFixpoint : Primitive {
    val signature: Signature = Signature("groundedFixpoint", 0)

    override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
        arg2pScope {
            val arguments = request.storedArguments()
            val position = arguments.withIndex().associate { (index, argument) -> argument.first to index }
            val graph = request.attackGraph()
            val inIds = request.labelledIds("in").toMutableSet()
            val outIds = request.labelledIds("out").toMutableSet()

            // an argument of the graph not labelled yet
            fun unlabelled(id: Term) = id in position && id !in inIds && id !in outIds

            // the attackers not OUT of each unlabelled argument
            val pending = HashMap<Term, Int>()
            arguments.forEach { (id, _) ->
                if (unlabelled(id)) pending[id] = graph.attackers[id].orEmpty().count { it !in outIds }
            }

            fun label(
                ids: Collection<Term>,
                name: String,
            ): List<Term> {
                val sorted = ids.sortedBy { position.getValue(it) }
                if (sorted.isEmpty()) return sorted
                request.labelArguments(listOf(Pair(name, sorted.map { id -> arguments[position.getValue(id)] })))
                return sorted
            }

            // the arguments to label IN, and those to label OUT, in the next round
            var candidatesIn: Collection<Term> = pending.filterValues { it == 0 }.keys
            var candidatesOut: Collection<Term> =
                arguments.map { it.first }.filter { id ->
                    unlabelled(id) &&
                        graph.attackers[id].orEmpty().any { it in inIds }
                }
            do {
                val newIn = label(candidatesIn.filter { unlabelled(it) && pending[it] == 0 }.toSet(), "in")
                inIds += newIn
                val attackedByIn = newIn.flatMap { graph.attacked[it].orEmpty() }
                val newOut = label((candidatesOut + attackedByIn).filter { unlabelled(it) }.toSet(), "out")
                outIds += newOut
                val released = LinkedHashSet<Term>()
                newOut.forEach { out ->
                    graph.attacked[out].orEmpty().forEach { target ->
                        val count = pending[target] ?: return@forEach
                        pending[target] = count - 1
                        if (count == 1) released += target
                    }
                }
                candidatesIn = released
                candidatesOut = emptyList()
            } while (newIn.isNotEmpty() || newOut.isNotEmpty())
            label(arguments.map { it.first }.filter { unlabelled(it) }, "und")
        }
        return sequenceOf(request.replyWith(true))
    }
}
