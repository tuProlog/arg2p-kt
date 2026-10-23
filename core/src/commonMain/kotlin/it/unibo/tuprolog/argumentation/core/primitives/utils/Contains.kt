package it.unibo.tuprolog.argumentation.core.primitives.utils

import it.unibo.tuprolog.argumentation.core.runtime.Arg2pUnificator
import it.unibo.tuprolog.core.List
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.exception.error.TypeError
import it.unibo.tuprolog.solve.primitive.Primitive
import it.unibo.tuprolog.solve.primitive.Solve

/**
 * contains(?Element, +List): an element of List matches Element (without binding it)
 */
object Contains : Primitive {
    val signature: Signature = Signature("contains", 2)

    override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
        val elem: Term = request.arguments[0]
        val list: Term = request.arguments[1]

        if (list !is List) {
            throw TypeError.forGoal(
                request.context,
                request.signature,
                TypeError.Expected.LIST,
                list,
            )
        }

        return sequence {
            yield(
                request.replyWith(list.toList().any { Arg2pUnificator.match(it, elem) }),
            )
        }
    }
}
