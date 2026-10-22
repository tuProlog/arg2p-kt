package it.unibo.tuprolog.argumentation.core.engine.stdlib.primitive

import it.unibo.tuprolog.argumentation.core.runtime.Arg2pUnificator
import it.unibo.tuprolog.core.Cons
import it.unibo.tuprolog.core.Integer
import it.unibo.tuprolog.core.List
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.Var
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.exception.error.DomainError
import it.unibo.tuprolog.solve.exception.error.TypeError
import it.unibo.tuprolog.solve.primitive.BinaryRelation
import it.unibo.tuprolog.solve.primitive.Solve
import it.unibo.tuprolog.solve.primitive.Solve.Response

/**
 * `length(?List, ?Length)` (arg2p: tuProlog 3 has none): Length is the number of items of List. A partial list (its
 * tail a variable) is completed with fresh variables to the given length, or, with no length given, to each length
 * in turn from the items it has.
 */
object Length : BinaryRelation<ExecutionContext>("length") {
    override fun Solve.Request<ExecutionContext>.computeAll(
        first: Term,
        second: Term,
    ): Sequence<Response> {
        if (second !is Var && second !is Integer) {
            throw TypeError.forArgument(context, signature, TypeError.Expected.INTEGER, second, 1)
        }
        if (second is Integer && second.intValue.signum < 0) {
            throw DomainError.forArgument(context, signature, DomainError.Expected.NOT_LESS_THAN_ZERO, second, 1)
        }
        var items = 0
        var tail = first
        while (tail is Cons) {
            items++
            tail = tail.tail
        }
        return when {
            tail.isEmptyList -> sequenceOf(unifying(second, Integer.of(items)))
            tail !is Var -> throw TypeError.forArgument(context, signature, TypeError.Expected.LIST, first, 0)
            second is Integer -> {
                val length = second.intValue.toInt()
                if (length < items) emptySequence() else sequenceOf(unifying(tail, freshList(length - items)))
            }
            else ->
                generateSequence(items) { it + 1 }.map { length ->
                    unifyingBoth(tail, freshList(length - items), second, Integer.of(length))
                }
        }
    }

    private fun freshList(size: Int): Term = List.of(List(size) { Var.anonymous() })

    private fun Solve.Request<ExecutionContext>.unifying(
        term: Term,
        value: Term,
    ): Response = Arg2pUnificator.mgu(term, value).let { if (it.isSuccess) replyWith(it) else replyFail() }

    private fun Solve.Request<ExecutionContext>.unifyingBoth(
        tail: Term,
        items: Term,
        length: Term,
        value: Term,
    ): Response =
        Arg2pUnificator
            .mgu(List.of(tail, length), List.of(items, value))
            .let { if (it.isSuccess) replyWith(it) else replyFail() }
}
