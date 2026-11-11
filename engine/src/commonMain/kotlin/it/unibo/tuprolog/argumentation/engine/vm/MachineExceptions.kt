package it.unibo.tuprolog.argumentation.engine.vm

import it.unibo.tuprolog.argumentation.engine.natives.NativeCall.Companion.CONTINUE
import it.unibo.tuprolog.argumentation.engine.vm.Machine.Companion.CATCH
import it.unibo.tuprolog.argumentation.engine.vm.Machine.Companion.CATCH_EXIT
import it.unibo.tuprolog.argumentation.engine.vm.Machine.Companion.CP_ARGS
import it.unibo.tuprolog.argumentation.engine.vm.Machine.Companion.CP_CATCH_INACTIVE
import it.unibo.tuprolog.argumentation.engine.vm.Machine.Companion.CP_CATCH_NUMBER
import it.unibo.tuprolog.argumentation.engine.vm.Machine.Companion.CP_KIND
import it.unibo.tuprolog.argumentation.engine.vm.Machine.Companion.CP_SIZE
import it.unibo.tuprolog.argumentation.engine.vm.Machine.Companion.NONE
import it.unibo.tuprolog.argumentation.engine.vm.Machine.Halt
import it.unibo.tuprolog.argumentation.engine.vm.Machine.Raised
import it.unibo.tuprolog.argumentation.engine.vm.PrologExecutionContext
import it.unibo.tuprolog.argumentation.engine.words.Words.REF
import it.unibo.tuprolog.argumentation.engine.words.Words.STR
import it.unibo.tuprolog.argumentation.engine.words.tag
import it.unibo.tuprolog.argumentation.engine.words.word
import it.unibo.tuprolog.core.Atom
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.exception.LogicError
import it.unibo.tuprolog.solve.exception.ResolutionException
import it.unibo.tuprolog.solve.exception.error.MessageError
import it.unibo.tuprolog.solve.exception.error.SystemError
// Balls and exceptions: the ball of a standard error, catch/3 (a choice point the unwinding of a ball stops at, its goal
// followed by '$catch_exit'(N)), the unwinding of a ball to the catch whose catcher unifies with it, the resolution
// exception of a ball no catch took, the ball of an exception

/** The ball `error(Type, Context)` of a standard error raised by the builtin [where] */
internal fun Machine.error(
    type: Term,
    where: String,
): Term = Struct.of("error", type, Atom.of(where))

// catch(Goal, Catcher, Recovery): a choice point the unwinding of a ball stops at, then the goal followed by
// '$catch_exit'(N), which ends the catch when the goal exits
internal fun Machine.catch(): Int {
    val number = ++catches
    pushChoicePoint(CATCH, 0, 3, 0, continuation, number)
    cutBarrier = cpTop
    val goal = heap.deref(regs[0])
    // (an unbound goal is an instantiation error, raised by callTerm, with the catch active)
    if (tag(goal) == REF) return callTerm(goal)
    val numberWord = heap.intWord(number.toLong())
    val h = heap.alloc(5)
    heap.cells[h] = wCatchExit
    heap.cells[h + 1] = numberWord
    heap.cells[h + 2] = wComma
    heap.cells[h + 3] = goal
    heap.cells[h + 4] = word(STR, h)
    return callTerm(word(STR, h + 2))
}

// The goal of the catch numbered regs[0] exited: if it left no choice point the catch goes; otherwise it stays,
// inactive, under a CATCH_EXIT choice point that makes it active again when backtracking goes back into the goal
internal fun Machine.catchExit(): Int {
    settle()
    val number = face.intValue(heap.deref(regs[0])).toInt()
    var cp = cpTop - 1
    while (cp >= 0 && !(cps[cp * CP_SIZE + CP_KIND] == CATCH && cps[cp * CP_SIZE + CP_CATCH_NUMBER] == number)) cp--
    if (cp < 0) return CONTINUE
    if (cp == cpTop - 1) {
        popChoicePoint()
    } else {
        cps[cp * CP_SIZE + CP_CATCH_INACTIVE] = 1
        pushChoicePoint(CATCH_EXIT, cp, 0, 0, continuation)
    }
    return CONTINUE
}

// Unwinds the choice points above [cpBase] to the nearest catch/3 whose catcher unifies with the ball of [raised]: its
// recovery, for the run to call in its place (the cut barrier set for it), or NONE when there is none
internal fun Machine.unwind(
    raised: Raised,
    cpBase: Int,
): Int {
    if (delayedNext >= 0) dropDelayed()
    while (cpTop > cpBase) {
        val base = (cpTop - 1) * CP_SIZE
        if (cps[base + CP_KIND] != CATCH || cps[base + CP_CATCH_INACTIVE] != 0) {
            popChoicePoint()
            continue
        }
        restoreTop()
        val argsBase = cps[base + CP_ARGS]
        val catcher = cpArgs[argsBase + 1]
        val recovery = cpArgs[argsBase + 2]
        popChoicePoint()
        val ball = heap.fromTerm(raised.ball, HashMap())
        if (heap.unify(catcher, ball)) {
            cutBarrier = cpTop
            return recovery
        }
    }
    return NONE
}

/** The resolution exception of a ball no catch took */
internal fun Machine.haltOf(raised: Raised): ResolutionException {
    raised.exception?.let { return it }
    val context: ExecutionContext = PrologExecutionContext(environment)
    val ball = raised.ball
    return if (ball is Struct && ball.functor == "error" && ball.arity in 1..2 && ball[0] is Struct) {
        try {
            // (the message: the error, and a text as its context, its explanation, whatever tuProlog makes of them)
            val type = ball[0] as Struct
            val extra = if (ball.arity > 1) ball[1] else null
            val message = (extra as? Atom)?.let { "$type: ${it.value}" }
            LogicError.of(message = message, context = context, type = type, extraData = extra)
        } catch (_: IllegalArgumentException) {
            MessageError.of(ball, context)
        }
    } else {
        MessageError.of(ball, context)
    }
}

// The ball of a resolution exception, as catch/3 sees it
internal fun Machine.ballOf(exception: ResolutionException): Term =
    when (exception) {
        is MessageError -> exception.content
        is LogicError -> exception.errorStruct
        else -> throw Halt(exception)
    }

// The ball of an exception the code of a primitive raised: a resolution exception as it is, any other a
// system_error catch/3 can handle
internal fun Machine.raised(exception: Exception): Raised {
    val resolution =
        exception as? ResolutionException
            ?: SystemError.forUncaughtException(PrologExecutionContext(environment), exception)
    return Raised(ballOf(resolution), resolution)
}
