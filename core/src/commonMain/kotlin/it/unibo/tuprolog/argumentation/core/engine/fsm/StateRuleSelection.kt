// Forked from 2p-kt's solve-prolog 3.0.2 (Apache License 2.0): see README.md in this package
package it.unibo.tuprolog.argumentation.core.engine.fsm

import it.unibo.tuprolog.argumentation.core.engine.ChoicePointContext
import it.unibo.tuprolog.argumentation.core.engine.PrologExecutionContext
import it.unibo.tuprolog.argumentation.core.engine.stdlib.rule.Catch
import it.unibo.tuprolog.core.Clause
import it.unibo.tuprolog.core.Rule
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.exception.ResolutionException
import it.unibo.tuprolog.solve.exception.error.ExistenceError
import it.unibo.tuprolog.solve.exception.warning.MissingPredicate
import it.unibo.tuprolog.solve.extractSignature
import it.unibo.tuprolog.solve.flags.LastCallOptimization
import it.unibo.tuprolog.solve.flags.LastCallOptimization.ON
import it.unibo.tuprolog.solve.flags.Unknown
import it.unibo.tuprolog.solve.stdlib.magic.MagicCut
import it.unibo.tuprolog.theory.Theory
import it.unibo.tuprolog.utils.Cursor

/**
 * "Rule Selection", the clause-resolution counterpart of `StatePrimitiveSelection`, and where several ISO
 * special cases live: `true`/`!` succeed without touching the knowledge base at all (`!` additionally triggers
 * cut, see below); `fail`/`false`, and goals that don't exist anywhere, move to `StateBacktracking` or
 * `StateException` depending on the `Unknown` flag (`error`/`fail`/`warning`); the general case queries the
 * static/dynamic knowledge bases and the libraries' own theories, `freshCopy()`s the matching clauses to rename
 * variables apart, and moves to `StateRuleExecution` with the fresh rule stream as a new choice point.
 *
 * The formal paper describes cut abstractly as "prune the choice-point queue up to the parent goal's choice
 * point"; [computeCutLimit]/[performCut] implement something considerably more careful, because ISO cut
 * semantics are subtler than that:
 * - Cut is **transparent** through conjunction, disjunction and if-then (the [transparentToCut] signatures) --
 *   a cut inside a clause body's conjunction cuts choice points belonging to the *clause*, not to some
 *   imaginary choice point for the comma itself; [computeCutLimit] walks up through these control constructs to
 *   find the real enclosing procedure.
 * - Cut interacts with last-call optimization ([isTailRecursive]): a tail call to the same predicate reuses
 *   the current execution-context frame ([replaceWithChildAppendingRulesAndChoicePoints]) rather than growing
 *   the stack, an optimisation the formal model does not need to talk about since it treats the stack as
 *   unbounded.
 * - A `MagicCut` marker exists for cuts injected by other built-ins (e.g. `once/1`, if-then-else) that must cut
 *   back to the *caller's* choice point rather than the lexically enclosing clause, since those built-ins are
 *   themselves implemented as ordinary rules over this same state machine (see `it.unibo.tuprolog.argumentation.core.engine.stdlib.rule`).
 *
 * None of this changes the shape of the FSM: cut is still handled here, still ends up at `StateGoalSelection`,
 * still only manipulates the choice-point queue.
 */
data class StateRuleSelection(
    override val context: PrologExecutionContext,
    // arg2p: the current goal (callable) with the substitution applied, as primitive selection computed it
    val goal: Struct,
) : AbstractState(context) {
    companion object {
        private val catchSignature = Catch.signature

        private val transparentToCut =
            setOf(
                Signature(",", 2),
                Signature(";", 2),
                Signature("->", 2),
            )

        private sealed class CutLimit {
            abstract val depthToCut: Int
            abstract val procedure: Struct?

            open val isNone: Boolean
                get() = false

            object None : CutLimit() {
                override val depthToCut: Int = -1
                override val procedure: Struct? = null

                override val isNone: Boolean
                    get() = true
            }

            class Actual(
                override val depthToCut: Int,
                override val procedure: Struct?,
            ) : CutLimit()
        }
    }

    private val failureState: StateBacktracking
        get() = StateBacktracking(context.copy(step = nextStep()))

    private fun exceptionalState(exception: ResolutionException): StateException =
        StateException(exception, context.copy(step = nextStep()))

    private fun missingProcedure(
        ruleSources: () -> Sequence<Theory>,
        missing: Signature,
    ): State =
        when (val unknown = context.flags[Unknown]) {
            Unknown.FAIL -> failureState
            else -> {
                if (ruleSources().none { missing.toIndicator() in it }) {
                    when (unknown) {
                        Unknown.ERROR ->
                            exceptionalState(
                                ExistenceError.forProcedure(
                                    context = context,
                                    procedure = missing,
                                ),
                            )
                        Unknown.WARNING ->
                            failureState.also {
                                context.warnings.write(
                                    MissingPredicate(
                                        context = context,
                                        signature = missing,
                                    ),
                                )
                            }
                        else -> failureState
                    }
                } else {
                    failureState
                }
            }
        }

    private val ignoreState: StateGoalSelection
        get() =
            StateGoalSelection(
                context.copy(goals = context.goals.next, step = nextStep()),
            )

    private fun Term.isCut(): Boolean = this.isAtom && this.castToAtom().value == "!"

    private fun PrologExecutionContext.computeCutLimit(magicCut: Boolean = false): CutLimit {
        val cutLimit =
            if (magicCut) {
                this.pathToRoot.firstOrNull()
            } else {
                this.pathToRoot.firstOrNull { it.procedure?.extractSignature() !in transparentToCut }
            }
        return if (cutLimit == null) {
            CutLimit.None
        } else {
            CutLimit.Actual(cutLimit.depth, cutLimit.procedure)
        }
    }

    private fun cutCanBeSkipped(
        cutLimit: CutLimit,
        executionContext: PrologExecutionContext,
    ): Boolean =
        cutLimit.isNone ||
            cutLimit.depthToCut > executionContext.depth ||
            cutLimit.depthToCut == executionContext.depth &&
            cutLimit.procedure != executionContext.procedure

    private fun ChoicePointContext?.performCut(cutLimit: CutLimit): ChoicePointContext? =
        when {
            this === null -> {
                null
            }
            cutCanBeSkipped(cutLimit, executionContext!!) -> {
                this
            }
            else -> {
                val cutCandidates =
                    pathToRoot.filter {
                        it.executionContext!!.procedure == cutLimit.procedure
                    }
                if (cutCandidates.any()) {
                    cutCandidates
                        .firstOrNull {
                            it.executionContext!!.depth <= cutLimit.depthToCut
                        }?.parent
                } else {
                    this
                }
            }
        }

    private fun PrologExecutionContext.isTailRecursive(currentGoal: Struct): Boolean =
        goals.next.isOver &&
            flags[LastCallOptimization] == ON &&
            currentGoal.extractSignature().let { it == procedure?.extractSignature() && it != catchSignature }

    override fun computeNext(): State =
        with(context) {
            val construct = ControlConstructs.of(goal)

            when {
                goal.isTruth -> {
                    if (goal.isTrue) {
                        ignoreState
                    } else {
                        failureState
                    }
                }

                goal.isCut() -> {
                    val cutLimit = computeCutLimit(goal is MagicCut)
                    ignoreState.let {
                        it.copy(
                            context =
                                it.context.copy(
                                    choicePoints = it.context.choicePoints.performCut(cutLimit),
                                ),
                        )
                    }
                }

                // arg2p: the predicates the engine solves natively
                construct != null -> construct.select(context, goal, executionTime)

                // The clauses are selected once: the procedure is missing when none is selected
                // (arg2p: checking first whether any clause matches matched them all twice). A single fact is
                // resolved in this frame (see FactResolution).
                else -> {
                    val clauses = ProgramClauses.of(this).clausesFor(goal, unificator)
                    FactResolution.of(context, goal, clauses, nextStep()) ?: selectRules(clauses)
                }
            }
        }

    private fun selectRules(clauses: List<Clause>): State {
        val rules: Cursor<out Rule> = RuleCursor.of(clauses)
        return when {
            rules.isOver -> missingProcedure({ context.ruleSources() }, goal.extractSignature())
            context.isTailRecursive(goal) ->
                StateRuleExecution(context.replaceWithChildAppendingRulesAndChoicePoints(rules, goal))
            else -> StateRuleExecution(context.createChildAppendingRulesAndChoicePoints(rules, goal))
        }
    }

    // The theories of the program, only needed to report a missing procedure
    private fun PrologExecutionContext.ruleSources(): Sequence<Theory> = sequenceOf(libraries.asTheory(unificator), staticKb, dynamicKb)
}
