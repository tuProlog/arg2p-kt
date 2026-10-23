package it.unibo.tuprolog.argumentation.core.primitives.context

import it.unibo.tuprolog.argumentation.core.primitives.PrimitiveWithSignature
import it.unibo.tuprolog.argumentation.core.runtime.Arg2pUnificator
import it.unibo.tuprolog.argumentation.core.runtime.context.ContextBranches
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.Var
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.primitive.Solve

/**
 * context_closure(+Functor, +From, ?To): To is reachable from From through one or more Functor(X, Y) facts,
 * each reachable term once (breadth first)
 */
internal class ContextClosure(
    private val branches: ContextBranches,
) : PrimitiveWithSignature {
    override val signature = Signature("context_closure", 3)

    override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
        val functor = request.arguments[0].castToAtom().value
        val store = branches.selected
        val next = Var.of("Next")
        val visited = LinkedHashSet<Term>()
        val queue = ArrayDeque<Term>().apply { addLast(request.arguments[1]) }
        while (queue.isNotEmpty()) {
            val current = queue.removeFirst()
            store.solve(Struct.of(functor, current, next)).forEach { solution ->
                val reached = solution[next] ?: return@forEach
                if (visited.add(reached)) queue.addLast(reached)
            }
        }
        val target = request.arguments[2]
        return visited.asSequence().mapNotNull { reached ->
            Arg2pUnificator
                .mgu(target, reached)
                .takeIf { it.isSuccess }
                ?.let { request.replyWith(it) }
        }
    }
}
