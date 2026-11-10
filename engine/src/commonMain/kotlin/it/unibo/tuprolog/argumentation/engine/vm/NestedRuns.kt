package it.unibo.tuprolog.argumentation.engine.vm

import it.unibo.tuprolog.argumentation.engine.natives.NativeCall.Companion.CONTINUE
import it.unibo.tuprolog.argumentation.engine.natives.NativeCall.Companion.FAIL
import it.unibo.tuprolog.argumentation.engine.vm.Machine.Companion.BACKTRACK
import it.unibo.tuprolog.argumentation.engine.vm.Machine.Companion.BARRIER
import it.unibo.tuprolog.argumentation.engine.vm.Machine.Companion.CALL
import it.unibo.tuprolog.argumentation.engine.vm.Machine.Companion.NONE
import it.unibo.tuprolog.argumentation.engine.vm.Machine.Raised
import it.unibo.tuprolog.argumentation.engine.words.Words.LIST
import it.unibo.tuprolog.argumentation.engine.words.shifted
import it.unibo.tuprolog.argumentation.engine.words.word

/**
 * The solutions of a goal in a nested run of the machine [m], above a barrier choice point that restores the state before
 * the goal: findall/3, and the findall and the counting native primitives ask (their registers and their state kept across
 * the run). The instances of the template go to the bag of the copier, above those of the runs this one is nested in, their
 * roots here; once the run is over, onto the heap as a list. A client of the interpreter, as the solver is: it drives its
 * run loop, and owns no state of it.
 */
internal class NestedRuns(
    private val m: Machine,
) {
    // (the heap of the machine, held: one load less on each cell read)
    private val heap = m.heap

    // The roots of the instances in the bag, those of the nested runs above
    private var roots = IntArray(256)
    private var rootsTop = 0

    /** The list the last run collected */
    var collected = 0
        private set

    /** findall/3 of the words [template], [goal] and [result]: CONTINUE or FAIL */
    fun findall(
        template: Int,
        goal: Int,
        result: Int,
    ): Int {
        val pins = m.pins
        val pinnedResult = pins.pin(result)
        solutions(template, goal, Int.MAX_VALUE)
        val resultWord = pins.words[pinnedResult]
        pins.truncate(pinnedResult)
        return if (heap.unify(resultWord, collected)) CONTINUE else FAIL
    }

    /**
     * The solutions of [goal] up to [limit], on behalf of the native call at hand: its registers and its state kept across the
     * run (the natives the run calls have theirs). How many there were, the instances of [template] (NONE for none) as the
     * list [collected]
     */
    fun forNative(
        template: Int,
        goal: Int,
        limit: Int,
    ): Int {
        val natives = m.natives
        val pins = m.pins
        val regs = m.regs
        val arity = natives.arity
        val names = natives.names
        val fresh = natives.fresh
        val pinnedBase = pins.top
        for (i in 0 until arity) pins.pin(regs[i])
        natives.hold()
        val count =
            try {
                solutions(template, goal, limit)
            } finally {
                natives.release(names, fresh)
            }
        // (the registers again: the nested run may have grown them)
        val after = m.regs
        for (i in 0 until arity) after[i] = pins.words[pinnedBase + i]
        pins.truncate(pinnedBase)
        natives.restore(arity, names, fresh)
        return count
    }

    // The solutions of [goal] up to [limit]: how many, the instances of [template] (NONE for none) as the list [collected]
    private fun solutions(
        template: Int,
        goal: Int,
        limit: Int,
    ): Int {
        val pins = m.pins
        val copier = m.copier
        val pinnedBase = pins.top
        val pinnedTemplate = if (template == NONE) NONE else pins.pin(template)
        // The barrier choice point: the state before the goal, resumed at the continuation of the call
        m.pushChoicePoint(BARRIER, 0, 0, 0, m.continuation)
        val cpBase = m.cpTop
        m.continuation = m.stopAddress
        m.cutBarrier = m.cpTop
        var found = limit > 0 && m.run(cpBase, CALL, goal)
        // (what the nested runs of the goal leave in the bag is gone by the next instance)
        val bagBase = copier.top
        val rootsBase = rootsTop
        var count = 0
        try {
            while (found) {
                if (pinnedTemplate != NONE) {
                    val root = copier.copy(heap.deref(pins.words[pinnedTemplate]))
                    if (rootsTop == roots.size) roots = roots.copyOf(rootsTop * 2)
                    roots[rootsTop++] = root
                }
                count++
                found = count < limit && m.run(cpBase, BACKTRACK)
            }
        } catch (raised: Raised) {
            copier.top = bagBase
            rootsTop = rootsBase
            throw raised
        }
        // (the run stopped at the limit leaves choice points above the barrier: cut, then the barrier restores)
        m.cutTo(cpBase)
        m.restoreTop()
        m.popChoicePoint()
        pins.truncate(pinnedBase)
        if (pinnedTemplate == NONE) {
            collected = m.program.emptyList
            rootsTop = rootsBase
            return count
        }
        // The bag onto the heap, its addresses shifted
        val shift = copier.toHeap(bagBase)
        var list = m.program.emptyList
        for (k in count - 1 downTo 0) {
            val cell = heap.alloc(2)
            heap.cells[cell] = shifted(roots[rootsBase + k], shift)
            heap.cells[cell + 1] = list
            list = word(LIST, cell)
        }
        collected = list
        copier.top = bagBase
        rootsTop = rootsBase
        return count
    }
}
