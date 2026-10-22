package it.unibo.tuprolog.argumentation.core.libs.utils

import it.unibo.tuprolog.argumentation.core.libs.basic.Arg2pUnificator
import it.unibo.tuprolog.argumentation.core.libs.basic.contextAssertAll
import it.unibo.tuprolog.core.List
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.Tuple
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.exception.error.TypeError
import it.unibo.tuprolog.solve.primitive.Primitive
import it.unibo.tuprolog.solve.primitive.Solve

// Primitives iterating over lists, for the module utils

/**
 * for_each(?Element, +List, +Goal): Goal (all its solutions) for each element of List, as
 * findall(_, (member(Element, List), Goal), _), but without List in the goals of the iterations: with variable
 * tracking, every resolution step visits the terms of the goal it belongs to, so the list would make every step
 * as costly as it is long. Goal runs in this module: other modules are called through ::.
 */
object ForEach : Primitive {
    val signature: Signature = Signature("for_each", 3)

    override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
        val list = request.arguments[1]
        if (list !is List) {
            throw TypeError.forGoal(request.context, request.signature, TypeError.Expected.LIST, list)
        }
        // the iterations are solved one after the other, by the same solver
        val solver = request.subSolver()
        list.toList().forEach { item ->
            val iteration = Tuple.of(request.arguments[0], request.arguments[2]).freshCopy()
            val unifier = Arg2pUnificator.mgu(iteration[0], item)
            if (unifier.isSuccess) {
                solver.solve(iteration[1].apply(unifier).castToStruct()).forEach { if (it.isHalt) throw it.exception!! }
            }
        }
        return sequenceOf(request.replyWith(true))
    }
}

object AssertAll : Primitive {
    val signature: Signature = Signature("assert_all", 1)

    override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
        val list: Term = request.arguments[0]

        if (list !is List) {
            throw TypeError.forGoal(
                request.context,
                request.signature,
                TypeError.Expected.LIST,
                list,
            )
        }

        request.contextAssertAll(list.toList())

        return sequence {
            yield(
                request.replyWith(true),
            )
        }
    }
}

// Sorting utilities, natively implemented as they are invoked for every argument
