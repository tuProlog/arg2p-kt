package it.unibo.tuprolog.argumentation.core.primitives.utils

import it.unibo.tuprolog.argumentation.core.runtime.Arg2pUnificator
import it.unibo.tuprolog.core.List
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.exception.error.TypeError
import it.unibo.tuprolog.solve.primitive.Primitive
import it.unibo.tuprolog.solve.primitive.Solve

/** appendLists(Lists, Result): Result is the concatenation of the lists in Lists */
object AppendLists : Primitive {
    val signature: Signature = Signature("appendLists", 2)

    override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
        val lists: Term = request.arguments[0]

        if (lists !is List) {
            throw TypeError.forGoal(
                request.context,
                request.signature,
                TypeError.Expected.LIST,
                lists,
            )
        }

        val items =
            lists.toList().flatMap { list ->
                if (list !is List) {
                    throw TypeError.forGoal(
                        request.context,
                        request.signature,
                        TypeError.Expected.LIST,
                        list,
                    )
                }
                list.toList()
            }
        val unifier = Arg2pUnificator.mgu(request.arguments[1], List.of(items))
        return sequenceOf(if (unifier.isSuccess) request.replyWith(unifier) else request.replyFail())
    }
}
