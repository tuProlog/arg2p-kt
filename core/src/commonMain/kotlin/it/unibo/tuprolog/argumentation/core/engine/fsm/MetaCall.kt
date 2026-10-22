package it.unibo.tuprolog.argumentation.core.engine.fsm

import it.unibo.tuprolog.argumentation.core.engine.PrologExecutionContext
import it.unibo.tuprolog.argumentation.core.engine.stdlib.rule.Call
import it.unibo.tuprolog.argumentation.core.engine.stdlib.rule.NegationAsFailure
import it.unibo.tuprolog.core.Rule
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Truth
import it.unibo.tuprolog.core.Tuple
import it.unibo.tuprolog.core.prepareForExecution
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.TimeInstant
import it.unibo.tuprolog.solve.exception.ResolutionException
import it.unibo.tuprolog.solve.stdlib.magic.MagicCut
import it.unibo.tuprolog.solve.stdlib.primitive.EnsureExecutable
import it.unibo.tuprolog.utils.Cursor

/**
 * A meta-call of its single argument, a goal: checked executable as ensure_executable/1 does, then solved in a frame
 * of its own, opaque to cut, whose goals and choice point are given by [frame].
 */
internal abstract class MetaCall(
    functor: String,
) : ControlConstruct {
    final override val signature: Signature = Signature(functor, 1)

    /** The frame solving [goal], from [frame], the frame of its own whose current goal is [goal] */
    protected abstract fun frame(
        goal: Struct,
        frame: PrologExecutionContext,
    ): PrologExecutionContext

    final override fun select(
        context: PrologExecutionContext,
        goal: Struct,
        time: TimeInstant,
    ): State {
        val frame = context.createChild(goal)
        val check = frame.copy(goals = Struct.of(EnsureExecutable.functor, goal[0]).toGoals()).createChild()
        return try {
            ensureExecutable(check, time)
            StateGoalSelection(frame(goal, frame).copy(step = context.step + 1))
        } catch (exception: ResolutionException) {
            StateException(exception.updateLastContext(check.skipThrow()), context.copy(step = context.step + 1))
        }
    }

    // Runs ensure_executable/1, the current goal of [check], raising its errors
    private fun ensureExecutable(
        check: PrologExecutionContext,
        time: TimeInstant,
    ) {
        val request = check.toRequest(check.currentGoal!!.castToStruct(), EnsureExecutable.signature, time)
        EnsureExecutable.implementation.solve(request).first()
    }
}

/**
 * call/1, as its clause `call(G) :- ensure_executable(G), G`: the frame solves the goal, its choice point (with no
 * alternatives) is the one a cut in the goal finds.
 */
internal object CallConstruct : MetaCall(Call.functor) {
    override fun frame(
        goal: Struct,
        frame: PrologExecutionContext,
    ): PrologExecutionContext {
        val body = Rule.of(goal, goal[0]).prepareForExecution().body
        return frame.copy(goals = body.toBodyGoals()).appendChoicePoint(Cursor.empty())
    }
}

/**
 * \+/1, as its clauses `\+ G :- ensure_executable(G), call(G), '$cut', fail` and `\+ G :- true`: the frame tries the
 * goal, cutting its own choice point and failing if the goal succeeds; the choice point goes on with the second
 * clause.
 */
internal object NegationConstruct : MetaCall(NegationAsFailure.FUNCTOR) {
    override fun frame(
        goal: Struct,
        frame: PrologExecutionContext,
    ): PrologExecutionContext =
        frame
            .appendChoicePoint(RuleCursor.of(listOf(Rule.of(goal, Truth.TRUE))))
            .copy(goals = Tuple.of(Struct.of(Call.functor, goal[0]), MagicCut, Truth.FAIL).toGoals())
}
