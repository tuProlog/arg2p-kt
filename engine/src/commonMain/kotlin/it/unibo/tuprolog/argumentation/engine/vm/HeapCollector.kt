package it.unibo.tuprolog.argumentation.engine.vm

import it.unibo.tuprolog.argumentation.engine.vm.Machine.Companion.CP_FRAME
import it.unibo.tuprolog.argumentation.engine.vm.Machine.Companion.CP_HEAP_TOP
import it.unibo.tuprolog.argumentation.engine.vm.Machine.Companion.CP_SIZE
import it.unibo.tuprolog.argumentation.engine.vm.Machine.Companion.FRAME
import it.unibo.tuprolog.argumentation.engine.vm.Machine.Companion.FRAME_SLOTS
import it.unibo.tuprolog.argumentation.engine.vm.Machine.Companion.NONE
import it.unibo.tuprolog.argumentation.engine.words.Words
import it.unibo.tuprolog.argumentation.engine.words.Words.FUN
import it.unibo.tuprolog.argumentation.engine.words.Words.INT
import it.unibo.tuprolog.argumentation.engine.words.Words.LIST
import it.unibo.tuprolog.argumentation.engine.words.Words.REF
import it.unibo.tuprolog.argumentation.engine.words.Words.STR
import it.unibo.tuprolog.argumentation.engine.words.hasAddress
import it.unibo.tuprolog.argumentation.engine.words.payload
import it.unibo.tuprolog.argumentation.engine.words.tag
import it.unibo.tuprolog.argumentation.engine.words.word
import it.unibo.tuprolog.core.Var

/**
 * The collector of the heap of a machine, a sliding mark-compact: the cells reachable from the arguments of the call at
 * hand, from the frames reachable from the current one and from those the choice points saved, from the arguments the
 * choice points saved, from the trail and from the words pinned around a nested run are kept, in their order, and the
 * rest slides down.
 */
internal class HeapCollector(
    private val m: Machine,
) {
    // (the heap of the machine, held: one load less on each cell read)
    private val heap = m.heap

    /** Words or cells of the heap a part of the machine holds, which the collector keeps and moves */
    interface Roots {
        /** Each word held to [c] ([HeapCollector.markWord]), each cell ([HeapCollector.markCell]) */
        fun markRoots(c: HeapCollector)

        /** Each held again where the collection moved it ([HeapCollector.relocated], [HeapCollector.newAddress]) */
        fun relocateRoots(c: HeapCollector)
    }

    // The parts holding roots of their own (the interpreter's, below, are its registers, frames, choice points and calls;
    // the maps of the native calls are kept by relocateNativeCells)
    private val roots: List<Roots> = listOf(m.heap, m.pins)

    private var marks = IntArray(0)
    private var counts = IntArray(0)
    private var markStack = IntArray(1 shl 10)
    private var frameVisited = BooleanArray(0)

    /** How many times the heap was collected, and how many cells it held before and after the last collection */
    var collections = 0
        private set
    var collectedFrom = 0
        private set
    var collectedTo = 0
        private set

    /** Collects the heap cells no longer reachable, [arity] registers holding the arguments of the call at hand */
    fun collect(arity: Int) {
        collections++
        collectedFrom = heap.top
        val blocks = (heap.top + 31) ushr 5
        if (marks.size < blocks) marks = IntArray(maxOf(blocks, (heap.cells.size + 31) ushr 5))
        marks.fill(0, 0, blocks)
        if (counts.size <= blocks) counts = IntArray(maxOf(blocks + 1, counts.size * 2))
        // Marking: the cells reachable from the roots
        for (i in 0 until arity) markWord(m.regs[i])
        if (frameVisited.size < m.local.size) frameVisited = BooleanArray(m.local.size)
        // (the frames reachable are below the local stack in use: a frame is allocated above the one before it and above
        // those the choice points keep, and a choice point keeps its frame below its mark)
        val inUse = maxOf(m.frameEnd(m.frame), m.localMark())
        frameVisited.fill(false, 0, inUse)
        markFrames(m.frame)
        for (cp in 0 until m.cpTop) markFrames(m.cps[cp * CP_SIZE + CP_FRAME])
        for (i in 0 until m.cpArgsTop) markWord(m.cpArgs[i])
        for (r in roots) r.markRoots(this)
        for (call in m.calls) if (call is PrimitiveCall) for (c in call.cells) markCell(c)
        // The new addresses: the cells kept below each block
        var n = 0
        for (b in 0 until blocks) {
            counts[b] = n
            n += marks[b].countOneBits()
        }
        counts[blocks] = n
        collectedTo = n
        // The roots relocated
        for (i in 0 until arity) m.regs[i] = relocated(m.regs[i])
        frameVisited.fill(false, 0, inUse)
        relocateFrames(m.frame)
        for (cp in 0 until m.cpTop) relocateFrames(m.cps[cp * CP_SIZE + CP_FRAME])
        for (i in 0 until m.cpArgsTop) m.cpArgs[i] = relocated(m.cpArgs[i])
        for (r in roots) r.relocateRoots(this)
        for (call in m.calls) if (call is PrimitiveCall) for (i in call.cells.indices) call.cells[i] = newAddress(call.cells[i])
        relocateNativeCells()
        for (cp in 0 until m.cpTop) {
            val base = cp * CP_SIZE
            m.cps[base + CP_HEAP_TOP] = newAddress(m.cps[base + CP_HEAP_TOP])
        }
        // The cells kept slide down, their words relocated, their names with them
        var f = 0
        for (b in 0 until blocks) {
            var bits = marks[b]
            val base = b shl 5
            while (bits != 0) {
                val i = base + bits.countTrailingZeroBits()
                heap.cells[f] = relocated(heap.cells[i])
                heap.varInfo[f] = heap.varInfo[i]
                f++
                bits = bits and (bits - 1)
            }
        }
        heap.top = n
        heap.barrier = m.barrierOfTop()
        m.gcThreshold = maxOf(m.gcMinimum, 4 * n)
    }

    // The cells the variables of native calls were given, each map once: what no root keeps is gone
    private fun relocateNativeCells() {
        val named = ArrayList<HashMap<Int, Var>>()
        val keep = { names: HashMap<Int, Var>? -> if (names != null && named.none { it === names }) named.add(names) }
        val freshes = ArrayList<HashMap<Var, Int>>()
        val keepFresh = { fresh: HashMap<Var, Int>? -> if (fresh != null && freshes.none { it === fresh }) freshes.add(fresh) }
        m.natives.forEachMap(keep, keepFresh)
        for (call in m.calls) if (call is NativeAlternatives) keep(call.names)
        for (names in named) {
            val entries = names.entries.map { it.key to it.value }
            names.clear()
            for ((cell, variable) in entries) if (cell < collectedFrom && marked(cell)) names[newAddress(cell)] = variable
        }
        for (fresh in freshes) {
            val entries = fresh.entries.map { it.key to it.value }
            fresh.clear()
            for ((variable, cell) in entries) if (cell < collectedFrom && marked(cell)) fresh[variable] = newAddress(cell)
        }
    }

    // The slots of the frames from [start] up (each once)
    private fun markFrames(start: Int) {
        var e = start
        while (e != NONE && !frameVisited[e]) {
            frameVisited[e] = true
            val slots = m.local[e + FRAME_SLOTS]
            for (i in 0 until slots) markSlot(m.local[e + FRAME + i])
            e = m.local[e]
        }
    }

    private fun relocateFrames(start: Int) {
        var e = start
        while (e != NONE && !frameVisited[e]) {
            frameVisited[e] = true
            val slots = m.local[e + FRAME_SLOTS]
            for (i in 0 until slots) m.local[e + FRAME + i] = relocatedSlot(m.local[e + FRAME + i])
            e = m.local[e]
        }
    }

    // A slot may hold what a retry left behind, never to be read: an address above the top, or one whose cell has been
    // reused since. What it finds there is not followed if it is no term, and the slot is cleared if nothing else keeps it
    private fun markSlot(w: Int) {
        val a = payload(w)
        when (tag(w)) {
            REF -> if (a < heap.top) markWord(w)
            STR -> if (a < heap.top && tag(heap.cells[a]) == FUN) markWord(w)
            LIST -> if (a + 1 < heap.top) markWord(w)
            else -> Unit
        }
    }

    private fun relocatedSlot(w: Int): Int {
        if (!hasAddress(w)) return w
        val a = payload(w)
        return if (a >= collectedFrom || !marked(a)) UNSET else relocated(w)
    }

    private fun marked(c: Int): Boolean = (marks[c ushr 5] ushr (c and 31)) and 1 != 0

    private fun mark(c: Int) {
        marks[c ushr 5] = marks[c ushr 5] or (1 shl (c and 31))
    }

    // The new address of a cell kept, or of a mark [address] (the first cell at or after it kept): the cells kept
    // below it
    fun newAddress(address: Int): Int {
        if (address >= collectedFrom) return collectedTo
        val b = address ushr 5
        return counts[b] + (marks[b] and ((1 shl (address and 31)) - 1)).countOneBits()
    }

    fun relocated(w: Int): Int = if (hasAddress(w)) word(tag(w), newAddress(payload(w))) else w

    fun markCell(c: Int) {
        if (marked(c)) return
        mark(c)
        var sp = 0
        markStack[sp++] = c
        while (sp > 0) {
            val cell = markStack[--sp]
            val w = heap.cells[cell]
            when (tag(w)) {
                REF -> {
                    val a = payload(w)
                    if (!marked(a)) {
                        mark(a)
                        sp = push(sp, a)
                    }
                }
                STR -> {
                    val a = payload(w)
                    if (!marked(a)) {
                        mark(a)
                        val n = m.program.arity(heap.cells[a])
                        for (i in 1..n) {
                            if (!marked(a + i)) {
                                mark(a + i)
                                sp = push(sp, a + i)
                            }
                        }
                    }
                }
                LIST -> {
                    val a = payload(w)
                    for (i in 0..1) {
                        if (!marked(a + i)) {
                            mark(a + i)
                            sp = push(sp, a + i)
                        }
                    }
                }
                else -> Unit
            }
        }
    }

    private fun push(
        sp: Int,
        cell: Int,
    ): Int {
        if (sp == markStack.size) markStack = markStack.copyOf(sp * 2)
        markStack[sp] = cell
        return sp + 1
    }

    fun markWord(w: Int) {
        when (tag(w)) {
            REF -> markCell(payload(w))
            STR -> {
                val a = payload(w)
                if (!marked(a)) {
                    mark(a)
                    val n = m.program.arity(heap.cells[a])
                    for (i in 1..n) markCell(a + i)
                }
            }
            LIST -> {
                markCell(payload(w))
                markCell(payload(w) + 1)
            }
            else -> Unit
        }
    }

    private companion object {
        // The slots of a frame before the clause sets them
        const val UNSET = (0 shl Words.TAG_BITS) or INT
    }
}
