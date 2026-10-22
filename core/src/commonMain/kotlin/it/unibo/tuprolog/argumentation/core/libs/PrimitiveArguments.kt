package it.unibo.tuprolog.argumentation.core.libs

import it.unibo.tuprolog.argumentation.core.libs.basic.Arg2pUnificator
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.exception.error.TypeError
import it.unibo.tuprolog.solve.primitive.Solve
import it.unibo.tuprolog.core.List as LogicList

// Arguments and replies of the native primitives

/** Identifiers in the list argument at the given index */
internal fun Solve.Request<ExecutionContext>.ids(index: Int): List<Term> {
    val ids = arguments[index]
    if (ids !is LogicList) throw TypeError.forGoal(context, signature, TypeError.Expected.LIST, ids)
    return ids.toList()
}

/** A reply binding the argument at the given index to the list of values, if they unify */
internal fun Solve.Request<ExecutionContext>.replyList(
    index: Int,
    values: List<Term>,
): Solve.Response? =
    Arg2pUnificator
        .mgu(arguments[index], LogicList.of(values))
        .takeIf { it.isSuccess }
        ?.let { replyWith(it) }
