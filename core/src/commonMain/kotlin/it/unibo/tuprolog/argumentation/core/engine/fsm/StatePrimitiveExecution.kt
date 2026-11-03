// Forked from 2p-kt's solve-prolog 3.0.2 (Apache License 2.0): see README.md in this package
package it.unibo.tuprolog.argumentation.core.engine.fsm

import it.unibo.tuprolog.argumentation.core.engine.PrologExecutionContext
import it.unibo.tuprolog.core.Substitution
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.utils.Cursor

/**
 * "Primitive Execution": consumes exactly one response from the current primitive's response stream
 * ([PrologExecutionContext.primitives]). A successful response merges its substitution and returns to
 * `StateGoalSelection`; an empty stream moves to `StateBacktracking`; an exceptional response moves to
 * `StateException`.
 */
data class StatePrimitiveExecution(
    override val context: PrologExecutionContext,
) : AbstractState(context) {
    private fun PrologExecutionContext.copyFromCurrentPrimitive(
        goals: Cursor<out Term>? = null,
        procedureFromAncestor: Int = 0,
        bindings: Bindings? = null,
    ): PrologExecutionContext {
        val ctx = primitives.current?.let { apply(it.sideEffects) } ?: this
        return ctx.copy(
            goals = goals ?: this.goals,
            procedure = ancestor(procedureFromAncestor).procedure,
            primitives = Cursor.empty(),
            bindings = bindings ?: this.bindings,
            step = nextStep(),
        )
    }

    // The frame [generations] above this one (arg2p: by a loop, where a sequence of the path to the root was built)
    private fun PrologExecutionContext.ancestor(generations: Int): PrologExecutionContext {
        var frame = this
        repeat(generations) { frame = frame.parent!! }
        return frame
    }

    // arg2p: the answer of a primitive binds the variables of its goal, which had the substitution applied: it extends
    // the bindings (see Bindings.extended) unless it binds a variable the substitution does, merged then
    private fun merged(answer: Substitution): Bindings {
        val bindings = context.bindings
        val unifier = answer.takeIf { it.isSuccess }?.castToUnifier()
        return if (unifier != null && unifier.keys.none { it in bindings }) {
            bindings.extended(unifier)
        } else {
            Bindings.of(context.unificator.merge(context.substitution, answer, occurCheckEnabled = false) as Substitution.Unifier)
        }
    }

    override fun computeNext(): State =
        try {
            context.primitives.current?.solution?.whenIs(
                yes = {
                    StateGoalSelection(
                        context.copyFromCurrentPrimitive(
                            goals = context.goals.next,
                            procedureFromAncestor = 1,
                            bindings = merged(it.substitution),
                        ),
                    )
                },
                no = {
                    StateBacktracking(context.parent!!.copyFromCurrentPrimitive())
                },
                halt = {
                    StateException(
                        it.exception.updateLastContext(context.skipThrow()),
                        context.copyFromCurrentPrimitive(),
                    )
                },
                otherwise = { throw IllegalStateException("This should never happen") },
            ) ?: StateBacktracking(context.copyFromCurrentPrimitive())
        } catch (exception: Exception) {
            StateException(exception.asResolutionException(context).updateLastContext(context.skipThrow()), context.copy(step = nextStep()))
        }
}
