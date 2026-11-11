package it.unibo.tuprolog.argumentation.engine.vm

import it.unibo.tuprolog.argumentation.engine.natives.NativeCall.Companion.CONTINUE
import it.unibo.tuprolog.argumentation.engine.natives.NativeCall.Companion.FAIL
import it.unibo.tuprolog.argumentation.engine.vm.Machine.Companion.BARRIER
import it.unibo.tuprolog.argumentation.engine.vm.Machine.Companion.CATCH
import it.unibo.tuprolog.argumentation.engine.vm.Machine.Companion.CATCH_EXIT
import it.unibo.tuprolog.argumentation.engine.vm.Machine.Companion.CP_ARGS
import it.unibo.tuprolog.argumentation.engine.vm.Machine.Companion.CP_ARITY
import it.unibo.tuprolog.argumentation.engine.vm.Machine.Companion.CP_BARRIER
import it.unibo.tuprolog.argumentation.engine.vm.Machine.Companion.CP_CALLS
import it.unibo.tuprolog.argumentation.engine.vm.Machine.Companion.CP_CATCH_INACTIVE
import it.unibo.tuprolog.argumentation.engine.vm.Machine.Companion.CP_CONTINUATION
import it.unibo.tuprolog.argumentation.engine.vm.Machine.Companion.CP_FRAME
import it.unibo.tuprolog.argumentation.engine.vm.Machine.Companion.CP_HEAP_TOP
import it.unibo.tuprolog.argumentation.engine.vm.Machine.Companion.CP_KEY
import it.unibo.tuprolog.argumentation.engine.vm.Machine.Companion.CP_KIND
import it.unibo.tuprolog.argumentation.engine.vm.Machine.Companion.CP_LOCAL_TOP
import it.unibo.tuprolog.argumentation.engine.vm.Machine.Companion.CP_NEXT
import it.unibo.tuprolog.argumentation.engine.vm.Machine.Companion.CP_PINNED
import it.unibo.tuprolog.argumentation.engine.vm.Machine.Companion.CP_RESUME_AT
import it.unibo.tuprolog.argumentation.engine.vm.Machine.Companion.CP_SCOPE
import it.unibo.tuprolog.argumentation.engine.vm.Machine.Companion.CP_SIZE
import it.unibo.tuprolog.argumentation.engine.vm.Machine.Companion.CP_SOURCE
import it.unibo.tuprolog.argumentation.engine.vm.Machine.Companion.CP_THEORY_VISIBLE
import it.unibo.tuprolog.argumentation.engine.vm.Machine.Companion.FRAME
import it.unibo.tuprolog.argumentation.engine.vm.Machine.Companion.FRAME_BARRIER
import it.unibo.tuprolog.argumentation.engine.vm.Machine.Companion.FRAME_CONTINUATION
import it.unibo.tuprolog.argumentation.engine.vm.Machine.Companion.FRAME_SLOTS
import it.unibo.tuprolog.argumentation.engine.vm.Machine.Companion.MERGED
import it.unibo.tuprolog.argumentation.engine.vm.Machine.Companion.MERGED_FREE
import it.unibo.tuprolog.argumentation.engine.vm.Machine.Companion.NATIVE
import it.unibo.tuprolog.argumentation.engine.vm.Machine.Companion.NONE
import it.unibo.tuprolog.argumentation.engine.vm.Machine.Companion.PRIMITIVE
import it.unibo.tuprolog.argumentation.engine.vm.Machine.Companion.RESUME
import it.unibo.tuprolog.argumentation.engine.vm.Machine.Companion.STORED
import it.unibo.tuprolog.argumentation.engine.vm.program.ClauseCode
import it.unibo.tuprolog.argumentation.engine.vm.program.ClauseTable
// The control stacks of the machine: the frames of the clauses (the local stack), and the choice points, the last one
// delayed until something needs it (a call, a cut, a primitive: settle), pushed, restored on backtracking, cut. Functions of
// the machine, on its state (local, frame, cps, cpTop, cpArgs, the delayed choice point), as its own methods are.

internal fun Machine.frameEnd(e: Int): Int = if (e == NONE) 0 else e + FRAME + local[e + FRAME_SLOTS]

internal fun Machine.localMark(): Int = if (cpTop == 0) 0 else cps[(cpTop - 1) * CP_SIZE + CP_LOCAL_TOP]

internal fun Machine.allocate(slots: Int) {
    val e = maxOf(frameEnd(frame), localMark())
    if (e + FRAME + slots > local.size) local = local.copyOf(maxOf(local.size * 2, e + FRAME + slots))
    local[e] = frame
    local[e + FRAME_CONTINUATION] = continuation
    local[e + FRAME_BARRIER] = cutBarrier
    local[e + FRAME_SLOTS] = slots
    frame = e
}

@Suppress("NOTHING_TO_INLINE")
internal inline fun Machine.deallocate() {
    continuation = local[frame + FRAME_CONTINUATION]
    cutBarrier = local[frame + FRAME_BARRIER]
    frame = local[frame]
}

internal fun Machine.saveDelayedArguments(arity: Int) {
    reserveArguments(arity)
    copyWords(regs, 0, cpArgs, cpArgsTop, arity)
    delayedSaved = true
}

// Room for [arity] arguments saved above the top of cpArgs
private fun Machine.reserveArguments(arity: Int) {
    if (cpArgsTop + arity > cpArgs.size) cpArgs = cpArgs.copyOf(maxOf(cpArgs.size * 2, cpArgsTop + arity))
}

// The barrier of the heap the choice point on top sets: the bindings of the cells below it are trailed
internal fun Machine.barrierOfTop(): Int = if (cpTop > 0) cps[(cpTop - 1) * CP_SIZE + CP_HEAP_TOP] else 0

// A few words at most, for the most part: a loop, not an array copy of a length the JIT of the JVM does not know
internal fun Machine.copyWords(
    from: IntArray,
    fromAt: Int,
    to: IntArray,
    toAt: Int,
    n: Int,
) {
    var i = 0
    while (i < n) {
        to[toAt + i] = from[fromAt + i]
        i++
    }
}

internal fun Machine.delayChoicePoint(
    table: ClauseTable,
    candidates: IntArray?,
    functor: Int,
    entered: ClauseCode,
    next: Int,
    arity: Int,
    key: Int,
) {
    if (entered.guarded) delayedSaved = false else saveDelayedArguments(arity)
    if (!table.pinned) table.pinned = true
    delayedNext = next
    delayedTable = table
    delayedCandidates = candidates
    delayedFunctor = functor
    delayedArity = arity
    delayedKey = key
    delayedTop = heap.top
    delayedTrail = heap.trailTop
    delayedFrame = frame
    heap.barrier = heap.top
}

// The choice point delayed made, if any: the clause goes beyond what it can undo alone
@Suppress("NOTHING_TO_INLINE")
internal inline fun Machine.settle() {
    if (delayedNext >= 0) madeChoicePoint()
}

// What arms of [execute] do through calls, the reads staying out of it: a field two arms or more read on entry, C2 reads
// at every dispatch, for all of them
internal fun Machine.settleHere() = settle()

internal fun Machine.proceeded(): Int {
    settle()
    return continuation
}

internal fun Machine.cutNeck() = cutTo(cutBarrier)

// The alternative at [address] of the clause being run, delayed as well: a condition of tests costs no choice point
internal fun Machine.delayAlternative(address: Int) {
    settle()
    delayedNext = address
    delayedSaved = true
    delayedTable = null
    delayedArity = 0
    delayedKey = 0
    delayedTop = heap.top
    delayedTrail = heap.trailTop
    delayedFrame = frame
    heap.barrier = heap.top
}

// The choice point delayed, made: of clauses (the next one of its table), or of an alternative of the clause run (RESUME)
internal fun Machine.madeChoicePoint() {
    val table = delayedTable
    if (!delayedSaved) saveDelayedArguments(delayedArity)
    if (table == null) {
        writeChoicePoint(null, null, delayedTrail, delayedTop, delayedFrame, delayedNext, RESUME, 0, 0, delayedArity, delayedKey, MERGED)
    } else {
        val source = if (delayedFree) MERGED_FREE else MERGED
        writeChoicePoint(
            table,
            delayedCandidates,
            delayedTrail,
            delayedTop,
            delayedFrame,
            0,
            delayedFunctor,
            delayedNext,
            cutBarrier,
            delayedArity,
            delayedKey,
            source,
        )
    }
    cpArgsTop += delayedArity
    delayedNext = -1
}

// The record of a choice point on top of the others: what backtracking to it restores (the marks of the trail, of the heap and
// of the local stack, the frame, the continuation, the scope, the pinned words, the calls), and what it goes on with (its kind,
// where to resume, its next clause, ...), its arguments saved at the top of cpArgs
private fun Machine.writeChoicePoint(
    table: ClauseTable?,
    candidates: IntArray?,
    trailMark: Int,
    heapTop: Int,
    frameAt: Int,
    resumeAt: Int,
    kind: Int,
    next: Int,
    barrier: Int,
    arity: Int,
    key: Int,
    source: Int,
) {
    val base = cpTop * CP_SIZE
    if (base + CP_SIZE > cps.size) growChoicePoints()
    cpTables[cpTop] = table
    cpCandidates[cpTop] = candidates
    cps[base] = trailMark
    cps[base + CP_HEAP_TOP] = heapTop
    cps[base + CP_LOCAL_TOP] = maxOf(frameEnd(frameAt), localMark())
    cps[base + CP_FRAME] = frameAt
    cps[base + CP_CONTINUATION] = continuation
    cps[base + CP_RESUME_AT] = resumeAt
    cps[base + CP_KIND] = kind
    cps[base + CP_NEXT] = next
    cps[base + CP_BARRIER] = barrier
    cps[base + CP_ARGS] = cpArgsTop
    cps[base + CP_CALLS] = calls.size
    cps[base + CP_ARITY] = arity
    cps[base + CP_KEY] = key
    cps[base + CP_SCOPE] = scope.id
    cps[base + CP_THEORY_VISIBLE] = if (theoryVisible) 1 else 0
    cps[base + CP_PINNED] = pins.top
    cps[base + CP_SOURCE] = source
    cpTop++
}

internal fun Machine.dropDelayed() {
    delayedNext = -1
    heap.barrier = barrierOfTop()
}

// The next clause of the choice point delayed, the machine as the call left it
internal fun Machine.retryDelayed(): Boolean {
    heap.undoTrail(delayedTrail)
    heap.top = delayedTop
    frame = delayedFrame
    val table = delayedTable
    if (table == null) {
        pc = delayedNext
        dropDelayed()
        return true
    }
    val arity = delayedArity
    if (delayedSaved) copyWords(cpArgs, cpArgsTop, regs, 0, arity)
    return retryClauses(table, delayedCandidates, delayedNext, arity, cutBarrier, delayedKey, delayedFree, NONE)
}

internal fun Machine.growChoicePoints() {
    cps = cps.copyOf(cps.size * 2)
    cpTables = cpTables.copyOf(cpTables.size * 2)
    cpCandidates = cpCandidates.copyOf(cpCandidates.size * 2)
}

internal fun Machine.pushChoicePoint(
    functor: Int,
    nextClause: Int,
    arity: Int,
    barrier: Int,
    resumeAt: Int,
    key: Int = 0,
    source: Int = MERGED,
) {
    settle()
    writeChoicePoint(null, null, heap.trailTop, heap.top, frame, resumeAt, functor, nextClause, barrier, arity, key, source)
    reserveArguments(arity)
    copyWords(regs, 0, cpArgs, cpArgsTop, arity)
    cpArgsTop += arity
    heap.barrier = heap.top
}

// Restores the state saved by the top choice point (which stays)
internal fun Machine.restoreTop() {
    val base = (cpTop - 1) * CP_SIZE
    heap.undoTrail(cps[base])
    heap.top = cps[base + CP_HEAP_TOP]
    frame = cps[base + CP_FRAME]
    continuation = cps[base + CP_CONTINUATION]
    pc = cps[base + CP_RESUME_AT]
    if (scope.id != cps[base + CP_SCOPE]) scope = program.scope(cps[base + CP_SCOPE])
    theoryVisible = cps[base + CP_THEORY_VISIBLE] != 0
    pins.truncate(cps[base + CP_PINNED])
}

internal fun Machine.popChoicePoint() {
    cpTop--
    val base = cpTop * CP_SIZE
    cpArgsTop = cps[base + CP_ARGS]
    truncateCalls(cps[base + CP_CALLS])
    heap.barrier = barrierOfTop()
}

internal fun Machine.truncateCalls(size: Int) {
    while (calls.size > size) calls.removeAt(calls.size - 1)
}

internal fun Machine.cutTo(barrier: Int) {
    if (delayedNext >= 0) dropDelayed()
    if (cpTop > barrier) {
        val base = barrier * CP_SIZE
        cpArgsTop = cps[base + CP_ARGS]
        truncateCalls(cps[base + CP_CALLS])
        cpTop = barrier
        heap.barrier = barrierOfTop()
    }
}

/**
 * Resumes the latest choice point above [cpBase] that leads somewhere: false when none is left (the machine
 * then restored to the state of the oldest one popped, if any)
 */
internal fun Machine.backtrack(cpBase: Int): Boolean {
    if (delayedNext >= 0 && retryDelayed()) return true
    while (cpTop > cpBase) {
        restoreTop()
        val base = (cpTop - 1) * CP_SIZE
        val functor = cps[base + CP_KIND]
        val nextClause = cps[base + CP_NEXT]
        when (functor) {
            BARRIER -> {
                popChoicePoint()
                return false
            }
            RESUME -> {
                popChoicePoint()
                return true
            }
            CATCH -> popChoicePoint()
            CATCH_EXIT -> {
                // back into the goal of the catch: active again
                cps[nextClause * CP_SIZE + CP_CATCH_INACTIVE] = 0
                popChoicePoint()
            }
            PRIMITIVE -> {
                val call = calls[nextClause] as PrimitiveCall
                call.pending?.let {
                    popChoicePoint()
                    throw raised(it)
                }
                val response = primitives.nextResponse(call.responses)
                if (!primitives.hasNextResponse(call)) popChoicePoint()
                if (primitives.applyResponse(response, call)) return true
            }
            NATIVE -> {
                val alternatives = calls[nextClause] as NativeAlternatives
                natives.resume(popWithArguments(base), alternatives.names, heap.top)
                val outcome = guardingNative { alternatives.resume.resume(face) }
                when (outcome) {
                    FAIL -> Unit
                    CONTINUE -> {
                        pc = continuation
                        return true
                    }
                    else -> return true
                }
            }
            STORED -> {
                val call = calls[nextClause] as StoredCall
                val from = cps[base + CP_KEY]
                val arity = popWithArguments(base)
                if (stores.tryStored(call, from, arity, nextClause) != FAIL) return true
            }
            else -> if (retryTop(base, functor)) return true
        }
    }
    return false
}

// The arguments the choice point on top, at [base], saved, back in the registers, and the choice point popped: how many
private fun Machine.popWithArguments(base: Int): Int {
    val arity = cps[base + CP_ARITY]
    copyWords(cpArgs, cps[base + CP_ARGS], regs, 0, arity)
    popChoicePoint()
    return arity
}

// [backtrack] as far as the choice points of clauses and conditions go: false for [backtrack] to go on
internal fun Machine.backtrackHere(cpBase: Int): Boolean {
    if (delayedNext >= 0 && retryDelayed()) return true
    while (cpTop > cpBase) {
        val base = (cpTop - 1) * CP_SIZE
        val functor = cps[base + CP_KIND]
        if (functor == RESUME) {
            restoreTop()
            popChoicePoint()
            return true
        }
        if (functor < 0) return false
        restoreTop()
        if (retryTop(base, functor)) return true
    }
    return false
}

// The next clause of the choice point on top, at [base], with the clauses of [functor], the machine restored to it
@Suppress("NOTHING_TO_INLINE")
internal inline fun Machine.retryTop(
    base: Int,
    functor: Int,
): Boolean {
    val arity = cps[base + CP_ARITY]
    copyWords(cpArgs, cps[base + CP_ARGS], regs, 0, arity)
    val cp = cpTop - 1
    if (cpTables[cp] == null) selectTheoryClauses(functor, arity, cp, base)
    return retryClauses(
        cpTables[cp]!!,
        cpCandidates[cp],
        cps[base + CP_NEXT],
        arity,
        cps[base + CP_BARRIER],
        cps[base + CP_KEY],
        cps[base + CP_SOURCE] == MERGED_FREE,
        base,
    )
}
