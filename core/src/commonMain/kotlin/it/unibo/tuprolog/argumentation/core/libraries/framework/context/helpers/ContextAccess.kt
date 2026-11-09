package it.unibo.tuprolog.argumentation.core.libraries.framework.context.helpers

import it.unibo.tuprolog.argumentation.core.libraries.framework.context.Context
import it.unibo.tuprolog.argumentation.engine.vm.MachineCall
import it.unibo.tuprolog.argumentation.engine.vm.NativeCall
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.exception.error.ExistenceError
import it.unibo.tuprolog.solve.exception.error.TypeError
import it.unibo.tuprolog.solve.library.Runtime

// The context of the solver of a native call, for the primitives reaching it without solving goals

/** The context library of the solver, every arg2p solver has one (an existence error for context_check/1 otherwise) */
internal val NativeCall.contextLibrary: Context
    get() =
        (if (this is MachineCall) libraryValue(CONTEXT_OF) as Context? else Context.of(libraries))
            ?: throw ExistenceError.forProcedure(context, Signature("context_check", 1))

private val CONTEXT_OF: (Runtime) -> Any? = { Context.of(it) }

/** A new branch of the given one, made active (context_branch/2): false if there is no such branch */
internal fun NativeCall.contextBranch(
    from: Term,
    signature: Signature,
): Boolean {
    val branch = from.asInteger()?.intValue?.toInt() ?: throw TypeError.forGoal(context, signature, TypeError.Expected.INTEGER, from)
    return contextLibrary.branch(branch) != null
}
