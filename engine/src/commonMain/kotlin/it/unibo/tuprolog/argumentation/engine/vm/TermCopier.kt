package it.unibo.tuprolog.argumentation.engine.vm

import it.unibo.tuprolog.argumentation.engine.words.Words.LIST
import it.unibo.tuprolog.argumentation.engine.words.Words.REF
import it.unibo.tuprolog.argumentation.engine.words.Words.STR
import it.unibo.tuprolog.argumentation.engine.words.payload
import it.unibo.tuprolog.argumentation.engine.words.ref
import it.unibo.tuprolog.argumentation.engine.words.shifted
import it.unibo.tuprolog.argumentation.engine.words.tag
import it.unibo.tuprolog.argumentation.engine.words.word

/**
 * The terms copied off the heap, their unbound variables fresh: findall's instances and the records. A bag with its
 * addresses, as a stack: a copy goes above what is there, and whoever made it takes it away. A copy is made in one go,
 * nothing runs meanwhile
 */
internal class TermCopier(
    private val m: Machine,
) {
    var bag = IntArray(256)
    var bagInfo = IntArray(256)
    var top = 0

    // The variables met in the copy at hand: addresses on the heap and in the bag, in a table as they are few
    private val seen = IntArray(64)
    private var seenTop = 0
    private var seenMany: HashMap<Int, Int>? = null

    private fun allocBag(cells: Int): Int {
        if (top + cells > bag.size) {
            bag = bag.copyOf(maxOf(bag.size * 2, top + cells))
            bagInfo = bagInfo.copyOf(bag.size)
        }
        val start = top
        top += cells
        return start
    }

    private fun variable(address: Int): Int {
        val many = seenMany
        if (many != null) {
            many[address]?.let { return it }
        } else {
            var i = 0
            while (i < seenTop) {
                if (seen[i] == address) return seen[i + 1]
                i += 2
            }
        }
        val a = allocBag(1)
        bag[a] = ref(a)
        bagInfo[a] = m.varInfo[address]
        if (many != null) {
            many[address] = a
        } else if (seenTop < seen.size) {
            seen[seenTop++] = address
            seen[seenTop++] = a
        } else {
            // too many to look through
            val all = HashMap<Int, Int>()
            var i = 0
            while (i < seenTop) {
                all[seen[i]] = seen[i + 1]
                i += 2
            }
            all[address] = a
            seenMany = all
        }
        return a
    }

    /** What is above [from] on the heap: how much to add to its addresses */
    fun toHeap(
        from: Int,
        named: Boolean = true,
    ): Int {
        val n = top - from
        val base = m.alloc(n)
        val shift = base - from
        for (i in 0 until n) {
            val w = bag[from + i]
            m.heap[base + i] = shifted(w, shift)
            m.varInfo[base + i] = if (named && tag(w) == REF && payload(w) == from + i) bagInfo[from + i] else 0
        }
        return shift
    }

    /** What is above [from], addressed from there */
    fun relativeTo(from: Int): IntArray {
        val n = top - from
        return IntArray(n) { i -> shifted(bag[from + i], -from) }
    }

    /** A copy of the term [w0] on the bag: its word, with the addresses of the bag */
    fun copy(w0: Int): Int {
        seenTop = 0
        seenMany = null
        return copyTerm(w0)
    }

    private fun copyTerm(w0: Int): Int {
        val w = m.deref(w0)
        return when (tag(w)) {
            REF -> ref(variable(payload(w)))
            STR -> {
                val a = payload(w)
                val n = m.program.arity(m.heap[a])
                val s = allocBag(n + 1)
                bag[s] = m.heap[a]
                for (i in 1..n) {
                    val arg = copyTerm(m.heap[a + i])
                    bag[s + i] = arg
                }
                word(STR, s)
            }
            LIST -> {
                val first = allocBag(2)
                var s = first
                var a = payload(w)
                while (true) {
                    val head = copyTerm(m.heap[a])
                    bag[s] = head
                    val t = m.deref(m.heap[a + 1])
                    if (tag(t) == LIST) {
                        val s2 = allocBag(2)
                        bag[s + 1] = word(LIST, s2)
                        s = s2
                        a = payload(t)
                    } else {
                        val tail = copyTerm(t)
                        bag[s + 1] = tail
                        break
                    }
                }
                word(LIST, first)
            }
            else -> w
        }
    }
}
