package it.unibo.tuprolog.argumentation.core.libs.graph.labelling

import it.unibo.tuprolog.argumentation.core.dsl.arg2pScope
import it.unibo.tuprolog.argumentation.core.libs.PrologModule
import it.unibo.tuprolog.argumentation.core.libs.graph.attackGraph
import it.unibo.tuprolog.argumentation.core.libs.graph.contextAssertAll
import it.unibo.tuprolog.argumentation.core.libs.graph.contextInstances
import it.unibo.tuprolog.argumentation.core.libs.graph.labelled
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.primitive.Primitive
import it.unibo.tuprolog.solve.primitive.Solve

object GroundedLabellerOptimized : PrologModule("prolog.argumentation.graph.labelling.grounded", "grounded") {
    override val primitives: Map<Signature, Primitive>
        get() = mapOf(GroundedFixpoint.signature to GroundedFixpoint)

    override val prologRawTheory: String =
        """
        % Query mode: the label of an argument only depends on the arguments it depends on (directionality)
        locality(directional).

        argumentLabelling :-
            grounded_fixpoint.
        """.trimIndent()
}

/**
 * grounded_fixpoint: extends the IN/OUT labelling of the active context to the grounded fixpoint, labelling UND
 * the arguments left. Each round labels IN the arguments whose attackers are all OUT, then OUT the arguments
 * attacked by an IN one; the context is read once and only the new labels are asserted.
 */
object GroundedFixpoint : Primitive {
    val signature: Signature = Signature("grounded_fixpoint", 0)

    override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
        arg2pScope {
            val arguments =
                request
                    .contextInstances("clause"("arg"(`_`), "argument"(`_`)))
                    .map { it[0].castToStruct()[0] to it[1].castToStruct()[0] }
            val attackers = request.attackGraph().attackers
            val inIds = request.labelled("inId").toMutableSet()
            val outIds = request.labelled("outId").toMutableSet()

            fun unlabelled() = arguments.filter { (a, _) -> a !in inIds && a !in outIds }

            fun label(
                labelled: List<Pair<Term, Term>>,
                name: String,
            ) {
                if (labelled.isEmpty()) return
                val facts = labelled.flatMap { (a, x) -> listOf(name(x), "${name}Id"(a)) }
                request.contextAssertAll(facts)
            }

            do {
                val newIn = unlabelled().filter { (a, _) -> attackers[a].orEmpty().all { it in outIds } }
                label(newIn, "in")
                inIds += newIn.map { it.first }
                val newOut = unlabelled().filter { (a, _) -> attackers[a].orEmpty().any { it in inIds } }
                label(newOut, "out")
                outIds += newOut.map { it.first }
            } while (newIn.isNotEmpty() || newOut.isNotEmpty())
            label(unlabelled(), "und")
        }
        return sequenceOf(request.replyWith(true))
    }
}
