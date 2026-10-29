package it.unibo.tuprolog.argumentation.core.primitives.utils

import it.unibo.tuprolog.argumentation.core.runtime.context.contextAssertAll
import it.unibo.tuprolog.core.List
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.exception.error.TypeError
import it.unibo.tuprolog.solve.primitive.Primitive
import it.unibo.tuprolog.solve.primitive.Solve

/**
 * assertAll(+Clauses): each of Clauses asserted in the context
 */
internal object AssertAll : Primitive {
    val signature: Signature = Signature("assertAll", 1)

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
