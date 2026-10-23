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
        substitution: Substitution? = null,
    ): PrologExecutionContext {
        val ctx = primitives.current?.let { apply(it.sideEffects) } ?: this
        return ctx.copy(
            goals = goals ?: this.goals,
            procedure = pathToRoot.drop(procedureFromAncestor).map { it.procedure }.first(),
            primitives = Cursor.empty(),
            substitution = (substitution ?: this.substitution) as Substitution.Unifier,
            step = nextStep(),
        )
    }

    // arg2p: the answer of a primitive binds the variables of its goal, which had the substitution applied: it extends
    // the substitution (see extendedWith) unless it binds a variable the substitution does, merged then
    private fun merged(answer: Substitution): Substitution {
        val substitution = context.substitution
        val unifier = answer.takeIf { it.isSuccess }?.castToUnifier()
        return if (unifier != null && unifier.keys.none { it in substitution }) {
            substitution.extendedWith(unifier)
        } else {
            context.unificator.merge(substitution, answer, occurCheckEnabled = false)
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
                            substitution = merged(it.substitution),
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
