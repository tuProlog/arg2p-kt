package it.unibo.tuprolog.argumentation.core.runtime.context

import it.unibo.tuprolog.argumentation.core.runtime.Arg2pUnificator
import it.unibo.tuprolog.core.Integer
import it.unibo.tuprolog.core.List
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.Var
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.exception.error.InstantiationError
import it.unibo.tuprolog.solve.exception.error.TypeError
import it.unibo.tuprolog.solve.primitive.Solve

// The arguments of the primitives of the context, and their answers

// The branch an argument of the request names: an integer (an instantiation or a type error otherwise)
internal fun Solve.Request<ExecutionContext>.branchArgument(index: Int): Int =
    when (val term = arguments[index]) {
        is Var -> throw InstantiationError.forArgument(context, signature, term, index)
        is Integer -> term.intValue.toInt()
        else -> throw TypeError.forArgument(context, signature, TypeError.Expected.INTEGER, term, index)
    }

// The branches the list in an argument of the request names
internal fun Solve.Request<ExecutionContext>.branchesArgument(index: Int): Set<Int> {
    fun typeError(
        expected: TypeError.Expected,
        culprit: Term,
    ) = TypeError.forArgument(context, signature, expected, culprit, index)
    val list = arguments[index] as? List ?: throw typeError(TypeError.Expected.LIST, arguments[index])
    return list
        .toList()
        .map { (it as? Integer)?.intValue?.toInt() ?: throw typeError(TypeError.Expected.INTEGER, it) }
        .toSet()
}

// The answer of the request unifying one of its arguments with a term
internal fun Solve.Request<ExecutionContext>.replyUnifying(
    index: Int,
    term: Term,
): Solve.Response = Arg2pUnificator.mgu(arguments[index], term).let { if (it.isSuccess) replyWith(it) else replyFail() }
