package it.unibo.tuprolog.argumentation.core.libs.basic

import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.exception.error.ExistenceError
import it.unibo.tuprolog.solve.exception.error.TypeError
import it.unibo.tuprolog.solve.primitive.Solve

// The context of the solver of a request, for the native primitives, which reach it without solving goals

/** The context library of the solver, every arg2p solver has one (an existence error for context_all/2 otherwise) */
internal val Solve.Request<ExecutionContext>.contextLibrary: Context
    get() = Context.of(context) ?: throw ExistenceError.forProcedure(context, Signature("context_all", 2))

/** Instances of goal in the given branch (the active one by default, none for a branch that does not exist) */
internal fun Solve.Request<ExecutionContext>.contextInstances(
    goal: Struct,
    branch: Term? = null,
): List<Struct> {
    val index = branch?.let { it.asInteger()?.intValue?.toInt() ?: return emptyList() }
    return contextLibrary.instances(goal, index).map { it.castToStruct() }
}

/** A new branch of the given one, made active (context_branch/2): false if there is no such branch */
internal fun Solve.Request<ExecutionContext>.contextBranch(from: Term): Boolean {
    val branch = from.asInteger()?.intValue?.toInt() ?: throw TypeError.forGoal(context, signature, TypeError.Expected.INTEGER, from)
    return contextLibrary.branch(branch) != null
}

/** The terms asserted in the active branch, in order (context_assert_all/1) */
internal fun Solve.Request<ExecutionContext>.contextAssertAll(terms: List<Term>) = contextLibrary.assertAll(terms)
