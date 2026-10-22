// Forked from 2p-kt's solve-prolog 3.0.2 (Apache License 2.0): see README.md in this package
package it.unibo.tuprolog.argumentation.core.engine.fsm

import it.unibo.tuprolog.argumentation.core.engine.InlinedCall
import it.unibo.tuprolog.argumentation.core.engine.PrologExecutionContext
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.exception.ResolutionException
import it.unibo.tuprolog.solve.exception.error.InstantiationError
import it.unibo.tuprolog.solve.exception.error.TypeError
import it.unibo.tuprolog.solve.extractSignature
import it.unibo.tuprolog.utils.Cursor
import it.unibo.tuprolog.utils.cursor

/**
 * "Primitive Selection": looks up a `Primitive` for the current goal's `(functor, arity)` signature among the
 * loaded libraries. If found, builds a child [PrologExecutionContext], invokes the primitive, and moves to
 * `StatePrimitiveExecution` with the resulting response cursor already attached as a choice point; if not found,
 * falls through to `StateRuleSelection`. A malformed goal -- an unbound variable, or a term that isn't callable
 * (not a [it.unibo.tuprolog.core.Struct]) -- short-circuits straight to `StateException` here, before any lookup
 * is attempted.
 */
data class StatePrimitiveSelection(
    override val context: PrologExecutionContext,
) : AbstractState(context) {
    private fun exceptionalState(exception: ResolutionException): StateException =
        StateException(
            exception,
            context.copy(step = nextStep()),
        )

    override fun computeNext(): State =
        with(context) {
            val goal = currentGoal!!
            when {
                goal.isVar -> {
                    exceptionalState(
                        InstantiationError.forGoal(
                            context = context,
                            procedure = context.procedure!!.extractSignature(),
                            variable = goal.castToVar(),
                        ),
                    )
                }
                goal.isStruct -> {
                    val goalStruct = goal.castToStruct()
                    val signature = goalStruct.extractSignature()

                    if (libraries.hasPrimitive(signature)) {
                        val childContext = createChild()
                        try {
                            val request = childContext.toRequest(goalStruct, signature, executionTime)
                            val primitive =
                                libraries.primitives[signature]
                                    ?: error("Inconsistent behaviour of Library.contains and Library.get")
                            if (primitive is InlinedCall) {
                                // Solved in place, in a frame of its own (arg2p: rather than in another solver), with a
                                // choice point of its own: the one a cut in the goal finds, to cut what follows it
                                val inlining = primitive.inline(request)
                                StateGoalSelection(
                                    childContext
                                        .copy(
                                            goals = inlining.goal.toGoals(),
                                            libraries = inlining.libraries,
                                            inlined = true,
                                            scoped = true,
                                        ).appendRulesAndChoicePoints(Cursor.empty()),
                                )
                            } else {
                                val primitiveExecutions = primitive.solve(request).cursor()
                                StatePrimitiveExecution(childContext.appendPrimitivesAndChoicePoints(primitiveExecutions))
                            }
                        } catch (exception: ResolutionException) {
                            exceptionalState(exception.updateLastContext(childContext.skipThrow()))
                        }
                    } else {
                        StateRuleSelection(context.copy(step = nextStep()))
                    }
                }
                else -> {
                    exceptionalState(
                        TypeError.forGoal(
                            context = context,
                            procedure = context.procedure?.extractSignature() ?: Signature("?-", 1),
                            expectedType = TypeError.Expected.CALLABLE,
                            culprit = goal,
                        ),
                    )
                }
            }
        }

    override fun clone(context: PrologExecutionContext): StatePrimitiveSelection = copy(context = context)
}
