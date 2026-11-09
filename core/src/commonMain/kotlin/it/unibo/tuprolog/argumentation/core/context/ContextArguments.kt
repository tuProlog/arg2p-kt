package it.unibo.tuprolog.argumentation.core.context

import it.unibo.tuprolog.core.Integer
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
