// Forked from 2p-kt's solve-prolog 3.0.2 (Apache License 2.0): see README.md in this package
@file:JvmName("Utils")

package it.unibo.tuprolog.argumentation.core.engine.fsm

import it.unibo.tuprolog.argumentation.core.engine.PrologExecutionContext
import it.unibo.tuprolog.argumentation.core.engine.appendPrimitives
import it.unibo.tuprolog.argumentation.core.engine.appendRules
import it.unibo.tuprolog.argumentation.core.engine.stdlib.primitive.Throw
import it.unibo.tuprolog.core.Clause
import it.unibo.tuprolog.core.Rule
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.Tuple
import it.unibo.tuprolog.core.Var
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.TimeInstant
import it.unibo.tuprolog.solve.exception.ResolutionException
import it.unibo.tuprolog.solve.exception.error.SystemError
import it.unibo.tuprolog.solve.primitive.Solve
import it.unibo.tuprolog.utils.Cursor
import kotlin.jvm.JvmName

/**
 * Turns this term into the [Cursor] of goals `StateGoalSelection` iterates over: conjunctions (`,`/2 tuples, nested
 * or not) are flattened into their leaves, depth-first, and every variable goal is wrapped into `call(X)` so it goes
 * through the ISO callability check before execution (arg2p: into a list, see [ListCursor]).
 */
fun Term.toGoals(): Cursor<out Term> =
    if (this is Tuple) {
        ListCursor.of(ArrayList<Term>(4).also { addGoalsTo(it, prepared = false) })
    } else {
        ListCursor.of(listOf(if (this is Var) Struct.of("call", this) else this))
    }

/**
 * Adds this term, a goal or a conjunction of goals, to [into] as the goals to execute (arg2p): the conjunctions
 * flattened, each goal [preparedForExecution] if [prepared], otherwise a variable goal wrapped into `call(X)`
 */
internal fun Term.addGoalsTo(
    into: ArrayList<Term>,
    prepared: Boolean,
) {
    var current: Term = this
    while (current is Tuple) {
        current.left.addGoalsTo(into, prepared)
        current = current.right
    }
    into.add(
        when {
            prepared -> current.preparedForExecution()
            current is Var -> Struct.of("call", current)
            else -> current
        },
    )
}

/** The goals of the body of a clause, as [toGoals]: none for `true`, the body of a fact (arg2p) */
fun Term.toBodyGoals(): Cursor<out Term> = if (isTrue) Cursor.empty() else toGoals()

/**
 * This term, a body, prepared for execution as Clause.prepareForExecution prepares the body of a clause (arg2p,
 * without building the clause): the variables in the place of a goal are called through call/1
 */
internal fun Term.preparedForExecution(): Term =
    when {
        this is Var -> Struct.of("call", this)
        this is Struct && arity == 2 && functor in Clause.notableFunctors -> {
            val left = this[0].preparedForExecution()
            val right = this[1].preparedForExecution()
            if (left === this[0] && right === this[1]) this else Struct.of(functor, left, right)
        }
        else -> this
    }

/**
 * This context, its step advanced in place (arg2p): the transitions that keep the frame as it is (the goal chosen, then
 * the clauses of its predicate selected) count without copying it. Every other state copies the context it is given,
 * and the contexts the choice points keep are never the ones a state goes on with: none but this state reads the
 * context advanced.
 */
internal fun PrologExecutionContext.advanced(): PrologExecutionContext = apply { step++ }

/**
 * Pushes a new child frame onto the execution-context stack for the current goal, becoming its own
 * [PrologExecutionContext.goals] (arg2p: [currentGoal] is the current goal with the substitution applied, when the
 * caller already has it)
 */
fun PrologExecutionContext.createChild(currentGoal: Struct = this.currentGoal!!.castToStruct()): PrologExecutionContext =
    copy(
        goals = currentGoal.toGoals(),
        procedure = currentGoal,
        parent = this,
        depth = depth + 1,
        step = step + 1,
        inlined = false,
    )

/**
 * Last-call-optimization counterpart of [createChild]: reuses the current frame for the current goal (incrementing
 * [PrologExecutionContext.depth] without growing the [PrologExecutionContext.parent] chain), used by
 * `StateRuleSelection` for tail calls to avoid an unbounded execution-context stack.
 */
fun PrologExecutionContext.replaceWithChild(currentGoal: Struct = this.currentGoal!!.castToStruct()): PrologExecutionContext =
    copy(
        goals = currentGoal.toGoals(),
        procedure = currentGoal,
        depth = depth + 1,
        step = step + 1,
    )

/**
 * Attaches [rules] as this context's [PrologExecutionContext.rules] cursor and records a matching
 * [it.unibo.tuprolog.argumentation.core.engine.ChoicePointContext.Rules] onto [PrologExecutionContext.choicePoints], so the
 * remaining candidates (if any) can be retried later on backtracking.
 */
fun PrologExecutionContext.appendRulesAndChoicePoints(rules: Cursor<out Rule>): PrologExecutionContext {
    val newChoicePointContext =
        if (rules.hasNext) {
            choicePoints.appendRules(rules.next, this)
        } else {
            choicePoints.appendRules(Cursor.empty(), this)
        }

    return copy(rules = rules, choicePoints = newChoicePointContext)
}

/**
 * Records a [it.unibo.tuprolog.argumentation.core.engine.ChoicePointContext.Rules] onto
 * [PrologExecutionContext.choicePoints] whose [alternatives] resolve the current goal of this context on backtracking
 * (arg2p): for the frames whose first alternative is not a clause (see [ControlConstruct])
 */
fun PrologExecutionContext.appendChoicePoint(alternatives: Cursor<out Rule>): PrologExecutionContext =
    copy(choicePoints = choicePoints.appendRules(alternatives, this))

/**
 * This exception, raised by the code of a primitive, as a resolution exception (arg2p): a Kotlin exception becomes a
 * system_error, which catch/3 can handle, rather than crashing the resolution
 */
fun Exception.asResolutionException(context: ExecutionContext): ResolutionException =
    this as? ResolutionException ?: SystemError.forUncaughtException(context, this)

/**
 * The answers of a primitive, an error raised computing one of them becoming an answer raising it (arg2p): the
 * engine computes an answer ahead (to know whether the call leaves alternatives), but the error is only raised if the
 * resolution backtracks into it, as an error of that answer
 */
fun Sequence<Solve.Response>.raisingErrorsInTurn(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> =
    sequence {
        val answers = iterator()
        while (true) {
            val answer =
                try {
                    if (answers.hasNext()) answers.next() else null
                } catch (exception: Exception) {
                    request.replyException(exception.asResolutionException(request.context))
                } ?: break
            yield(answer)
            if (answer.solution.isHalt) break
        }
    }

/** The context an error raised in this one is reported in: the nearest one that is not throw/1 */
fun PrologExecutionContext.skipThrow(): ExecutionContext = pathToRoot.first { it.procedure?.functor != Throw.functor }

/** [Solve.Response] analogue of [appendRulesAndChoicePoints], recording a [it.unibo.tuprolog.argumentation.core.engine.ChoicePointContext.Primitives] instead. */
fun PrologExecutionContext.appendPrimitivesAndChoicePoints(primitiveExecutions: Cursor<out Solve.Response>): PrologExecutionContext {
    val newChoicePointContext =
        if (primitiveExecutions.hasNext) {
            choicePoints.appendPrimitives(primitiveExecutions.next, this)
        } else {
            choicePoints.appendPrimitives(Cursor.empty(), this)
        }

    return copy(primitives = primitiveExecutions, choicePoints = newChoicePointContext)
}

/** Combines [createChild] and [appendRulesAndChoicePoints]: the usual way `StateRuleSelection` moves into a non-tail rule call. */
fun PrologExecutionContext.createChildAppendingRulesAndChoicePoints(
    rules: Cursor<out Rule>,
    currentGoal: Struct = this.currentGoal!!.castToStruct(),
): PrologExecutionContext = createChild(currentGoal).appendRulesAndChoicePoints(rules)

/** Combines [replaceWithChild] and [appendRulesAndChoicePoints]: how `StateRuleSelection` moves into a last-call-optimized (tail) rule call. */
fun PrologExecutionContext.replaceWithChildAppendingRulesAndChoicePoints(
    rules: Cursor<out Rule>,
    currentGoal: Struct = this.currentGoal!!.castToStruct(),
): PrologExecutionContext = replaceWithChild(currentGoal).appendRulesAndChoicePoints(rules)

/** Builds the [Solve.Request] passed to a `Primitive`'s `solve` from this context, [goal] and [signature], at [startTime]. */
fun PrologExecutionContext.toRequest(
    goal: Struct,
    signature: Signature,
    startTime: TimeInstant,
) = Solve.Request(signature, goal.args, this, startTime)

// The variables of a term, collected by a plain recursion (Term.variables nests a lazy sequence for each argument),
// skipping its ground parts (whose groundness is known: lists build it, other structures compute it once)
internal fun Term.collectVariables(into: MutableSet<Var>) {
    when (this) {
        is Var -> into.add(this)
        is Struct -> if (!isGround) for (i in 0 until arity) this[i].collectVariables(into)
        else -> Unit
    }
}

/**
 * The signature of a callable (arg2p): built from its functor and arity alone, as extractSignature() builds the
 * indicator term of the callable (an atom, an integer and their lazy values) only to read them back
 */
internal val Struct.signatureOf: Signature get() = Signature(functor, arity)
