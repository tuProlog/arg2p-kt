package it.unibo.tuprolog.argumentation.engine.vm

import it.unibo.tuprolog.argumentation.engine.words.Words.ATOM
import it.unibo.tuprolog.argumentation.engine.words.Words.BIG
import it.unibo.tuprolog.argumentation.engine.words.Words.FLT
import it.unibo.tuprolog.argumentation.engine.words.Words.INT
import it.unibo.tuprolog.argumentation.engine.words.Words.LIST
import it.unibo.tuprolog.argumentation.engine.words.Words.REF
import it.unibo.tuprolog.argumentation.engine.words.Words.STR
import it.unibo.tuprolog.argumentation.engine.words.payload
import it.unibo.tuprolog.argumentation.engine.words.tag
import it.unibo.tuprolog.core.Real
import it.unibo.tuprolog.core.TermComparator
import org.gciatto.kt.math.BigInteger

/**
 * The standard order of tuProlog over the terms on the heap of a machine, and the sorts by it. The atoms are compared by
 * rank, each collated once.
 */
internal class TermOrder(
    private val m: Machine,
) {
    // (the heap of the machine, held: one load less on each cell read)
    private val heap = m.heap

    // The atoms ordered so far by the standard order: their ranks by atom (0 for one not met yet), the atoms of each
    // rank. Two atoms with the same rank are equal for the order. An atom met costs a binary search among them
    private var atomRanks = IntArray(64)
    private var rankedAtoms = IntArray(64)
    private var ranks = 0
    private var metAtoms = IntArray(64)
    private var met = 0

    private fun meet(
        atom: Int,
        rank: Int,
    ): Int {
        atomRanks[atom] = rank
        if (met == metAtoms.size) metAtoms = metAtoms.copyOf(met * 2)
        metAtoms[met++] = atom
        return rank
    }

    private fun rankOf(atom: Int): Int {
        if (atom >= atomRanks.size) atomRanks = atomRanks.copyOf(maxOf(atom + 1, atomRanks.size * 2))
        val known = atomRanks[atom]
        if (known != 0) return known
        val name = m.program.symbols.atoms[atom]
        var low = 0
        var high = ranks - 1
        while (low <= high) {
            val middle = (low + high) ushr 1
            val order = compareAtomNames(name, m.program.symbols.atoms[rankedAtoms[middle]])
            when {
                order == 0 -> return meet(atom, middle + 1)
                order < 0 -> high = middle - 1
                else -> low = middle + 1
            }
        }
        // Below the others from [low] on, whose ranks move up
        if (ranks == rankedAtoms.size) rankedAtoms = rankedAtoms.copyOf(ranks * 2)
        rankedAtoms.copyInto(rankedAtoms, low + 1, low, ranks)
        rankedAtoms[low] = atom
        ranks++
        for (i in 0 until met) if (atomRanks[metAtoms[i]] > low) atomRanks[metAtoms[i]]++
        return meet(atom, low + 1)
    }

    // The atoms a sort meets for the first time: above a few, ranked all at once (rankBatch), one sort of their names and
    // one merge with the atoms ranked so far, rather than each by a binary search that renumbers the atoms met before it
    private var unranked = IntArray(64)
    private var unrankedTop = 0

    // The atoms of [w0] with no rank, collected (their rank -1 until rankBatch)
    private fun collectUnranked(w0: Int) {
        var w = heap.deref(w0)
        while (true) {
            when (tag(w)) {
                ATOM -> {
                    val atom = payload(w)
                    if (atom >= atomRanks.size) atomRanks = atomRanks.copyOf(maxOf(atom + 1, atomRanks.size * 2))
                    if (atomRanks[atom] == 0) {
                        atomRanks[atom] = -1
                        if (unrankedTop == unranked.size) unranked = unranked.copyOf(unrankedTop * 2)
                        unranked[unrankedTop++] = atom
                    }
                    return
                }
                STR -> {
                    val a = payload(w)
                    val n = m.program.arity(heap.cells[a])
                    for (i in 1 until n) collectUnranked(heap.cells[a + i])
                    w = heap.deref(heap.cells[a + n])
                }
                LIST -> {
                    val a = payload(w)
                    collectUnranked(heap.cells[a])
                    w = heap.deref(heap.cells[a + 1])
                }
                else -> return
            }
        }
    }

    // The atoms collectUnranked collected, ranked: the same ranks rankOf gives them one by one, up to a renumbering of all
    // the ranks that keeps their order (the ranks are only compared). Below a few, none is ranked here
    private fun rankBatch() {
        val count = unrankedTop
        unrankedTop = 0
        if (count == 0) return
        if (count < RANKED_ONE_BY_ONE) {
            // (left to rankOf, which ranks only the atoms a comparison reaches)
            for (i in 0 until count) atomRanks[unranked[i]] = 0
            return
        }
        val names = m.program.symbols.atoms
        val fresh = unranked.copyOf(count).sortedWith { x, y -> compareAtomNames(names[x], names[y]) }
        val merged = IntArray(ranks + count)
        val oldToNew = IntArray(ranks + 1)
        val freshRanks = IntArray(count)
        var m = 0
        var i = 0
        var j = 0
        while (i < ranks || j < count) {
            val order =
                when {
                    i == ranks -> 1
                    j == count -> -1
                    else -> compareAtomNames(names[rankedAtoms[i]], names[fresh[j]])
                }
            if (order <= 0) {
                merged[m++] = rankedAtoms[i]
                oldToNew[++i] = m
            } else {
                // (a name equal to the last one ranked takes its rank)
                if (m == 0 || compareAtomNames(names[merged[m - 1]], names[fresh[j]]) != 0) merged[m++] = fresh[j]
                freshRanks[j++] = m
            }
        }
        for (k in 0 until met) atomRanks[metAtoms[k]] = oldToNew[atomRanks[metAtoms[k]]]
        rankedAtoms = merged
        ranks = m
        for (k in 0 until count) meet(fresh[k], freshRanks[k])
    }

    /** The standard order of the atoms [a] and [b] */
    fun atomOrder(
        a: Int,
        b: Int,
    ): Int {
        rankOf(a)
        rankOf(b)
        // (the rank of the first may have moved up when the second was met)
        return atomRanks[a].compareTo(atomRanks[b])
    }

    fun sorted(items: IntArray): IntArray? {
        var integers = true
        for (i in items.indices) {
            val item = heap.deref(items[i])
            if (tag(item) != INT) {
                integers = false
                break
            }
        }
        if (integers) {
            // (the words of the small integers are ordered as they are)
            val sorted = IntArray(items.size) { heap.deref(items[it]) }
            sorted.sort()
            return sorted
        }
        for (item in items) if (!HeapTerms.ground(m, item)) return null
        for (item in items) collectUnranked(item)
        rankBatch()
        val sorted = items.copyOf()
        if (!inOrderAlready(sorted)) sortStably(sorted)
        return sorted
    }

    // Whether [items] end up in the standard order by a single pass: they are in it, or in the reverse of it (the lists
    // arg2p sorts mostly are, being read from a store newest first)
    private fun inOrderAlready(items: IntArray): Boolean {
        if (items.size < 2) return true
        var i = 1
        while (i < items.size && order(items[i - 1], items[i]) <= 0) i++
        if (i == items.size) return true
        if (i > 1) return false
        i = 1
        while (i < items.size && order(items[i - 1], items[i]) > 0) i++
        if (i < items.size) return false
        items.reverse()
        return true
    }

    // In the standard order, keeping the order of the equal ones: by insertion when few, merging otherwise
    private fun sortStably(items: IntArray) {
        if (items.size <= 12) {
            for (i in 1 until items.size) {
                val item = items[i]
                var at = i - 1
                while (at >= 0 && order(items[at], item) > 0) {
                    items[at + 1] = items[at]
                    at--
                }
                items[at + 1] = item
            }
            return
        }
        val left = items.copyOfRange(0, items.size / 2)
        val right = items.copyOfRange(items.size / 2, items.size)
        sortStably(left)
        sortStably(right)
        if (order(left[left.size - 1], right[0]) <= 0) {
            left.copyInto(items)
            right.copyInto(items, left.size)
            return
        }
        var l = 0
        var r = 0
        for (i in items.indices) {
            items[i] =
                if (r == right.size || (l < left.size && order(left[l], right[r]) <= 0)) left[l++] else right[r++]
        }
    }

    fun sortedDistinct(items: IntArray): IntArray? {
        val sorted = sorted(items) ?: return null
        var n = 0
        for (i in sorted.indices) {
            if (n == 0 || order(sorted[n - 1], sorted[i]) != 0) sorted[n++] = sorted[i]
        }
        return sorted.copyOf(n)
    }

    // The ranks of the standard order of tuProlog: reals before integers, atoms, compound terms
    private fun rank(t: Int): Int =
        when (t) {
            FLT -> 1
            INT, BIG -> 2
            ATOM -> 3
            else -> 4
        }

    // (an integer is a small one, or the compound of one no word holds, see Numbers)
    private fun integerOrder(
        x: Int,
        y: Int,
    ): Int {
        if (tag(x) == INT && tag(y) == INT) return payload(x).compareTo(payload(y))
        return bigOf(x).compareTo(bigOf(y))
    }

    private fun bigOf(w: Int): BigInteger = if (tag(w) == INT) BigInteger.of(payload(w).toLong()) else heap.bigIntegerOfNumber(w)

    // The float compound [w] (see Numbers) as a tuProlog float
    private fun realOf(w: Int): Real {
        val a = payload(w)
        val n = m.program.arity(heap.cells[a])
        return m.program.realOf(
            payload(heap.deref(heap.cells[a + 1])),
            payload(heap.deref(heap.cells[a + 2])),
            IntArray(n - 2) {
                payload(
                    heap.deref(
                        heap.cells[
                            a +
                                3 +
                                it,
                        ],
                    ),
                )
            },
        )
    }

    /**
     * The terms in the standard order of tuProlog (see [TermComparator.DefaultComparator]): negative, zero, positive,
     * or [UNORDERED] when it depends on a variable
     */
    fun order(
        x0: Int,
        y0: Int,
    ): Int {
        var x = heap.deref(x0)
        var y = heap.deref(y0)
        val program = m.program
        while (true) {
            if (x == y) return 0
            val tx = heap.typeTag(x)
            val ty = heap.typeTag(y)
            if (tx == REF || ty == REF) return UNORDERED
            val rx = rank(tx)
            val ry = rank(ty)
            if (rx != ry) return rx - ry
            when (rx) {
                1 -> return TermComparator.RealComparator.compare(realOf(x), realOf(y))
                2 -> return integerOrder(x, y)
                3 -> return atomOrder(payload(x), payload(y))
                else -> {
                    val a = payload(x)
                    val b = payload(y)
                    val fx = if (tx == LIST) 0 else heap.cells[a]
                    val fy = if (ty == LIST) 0 else heap.cells[b]
                    val nx = if (tx == LIST) 2 else program.arity(fx)
                    val ny = if (ty == LIST) 2 else program.arity(fy)
                    if (nx != ny) return nx - ny
                    if (fx != fy) {
                        val nameX = if (tx == LIST) "." else program.nameOfFunctorWord(fx)
                        val nameY = if (ty == LIST) "." else program.nameOfFunctorWord(fy)
                        val names = nameX.compareTo(nameY)
                        if (names != 0) return names
                    }
                    val first = if (tx == LIST) 0 else 1
                    val second = if (ty == LIST) 0 else 1
                    for (i in 0 until nx - 1) {
                        val argument = order(heap.cells[a + first + i], heap.cells[b + second + i])
                        if (argument != 0) return argument
                    }
                    x = heap.deref(heap.cells[a + first + nx - 1])
                    y = heap.deref(heap.cells[b + second + ny - 1])
                }
            }
        }
    }

    companion object {
        /** Two terms the standard order cannot tell here: a variable is met (the library orders them by name) */
        const val UNORDERED = Int.MIN_VALUE

        // Fewer atoms new to a sort than this are left to be ranked one by one as compared (see rankBatch)
        private const val RANKED_ONE_BY_ONE = 32
    }
}
