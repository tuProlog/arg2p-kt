package it.unibo.tuprolog.argumentation.engine.vm

import it.unibo.tuprolog.argumentation.engine.words.WordTable
import it.unibo.tuprolog.argumentation.engine.words.locked
import it.unibo.tuprolog.core.Clause

internal class ClauseCode(
    /** The keys of the arguments of the head (0: a variable, matching anything); [key] is the first */
    val keys: IntArray,
    /** The address of the first instruction */
    val start: Int,
    /** The clause as given */
    val source: Clause,
    /** Whether the clause is dynamic (retracted by equality) */
    val dynamic: Boolean,
    /** The keys of the arguments of its first argument, if that is compound and any is keyed; empty otherwise */
    val innerKeys: IntArray = Program.NO_KEYS,
    /** Whether it leaves its arguments in the registers up to a cut opening it, its head and tests changing no register */
    val guarded: Boolean = false,
    /**
     * Whether its head cannot fail, a neck cut following, once the first key matched and the arguments of this mask,
     * as bits, are unbound; -1 if it may. No alternative is then ever reached
     */
    val commitMask: Int = -1,
) {
    val key: Int get() = if (keys.isEmpty()) 0 else keys[0]

    /** Whether an argument beyond the first is keyed */
    val deep: Boolean get() = innerKeys.isNotEmpty() || keys.size > 1 && (1 until keys.size).any { keys[it] != 0 }
}

/**
 * The clauses of a predicate at some point, flat for the selection to read: once [pinned], a call still running on them
 * may come back to them, so they do not change, and that call sees them as they were when it was made
 */
internal class ClauseTable(
    val capacity: Int,
) {
    val clauses = arrayOfNulls<ClauseCode>(capacity)
    val firstKeys = IntArray(capacity)
    val starts = IntArray(capacity)
    var size = 0

    /** Whether a clause keys an argument beyond the first, or within it */
    var deep = false

    /** Whether a clause keys the arguments within its first argument */
    var innerDeep = false
    var pinned = false

    fun copied(toCapacity: Int): ClauseTable {
        val copy = ClauseTable(toCapacity)
        clauses.copyInto(copy.clauses, 0, 0, size)
        firstKeys.copyInto(copy.firstKeys, 0, 0, size)
        starts.copyInto(copy.starts, 0, 0, size)
        copy.size = size
        copy.deep = deep
        copy.innerDeep = innerDeep
        return copy
    }
}

/** The clauses of a predicate, and which of them a first argument may match */
internal class Predicate(
    val functor: Int,
) {
    /** The clauses now: what a choice point holds is never changed, the next change makes another (see [ClauseTable.pinned]) */
    var table = ClauseTable(4)
        private set

    /** The clauses, as a list */
    val clauses: List<ClauseCode> get() = table.let { t -> List(t.size) { t.clauses[it]!! } }

    private var all: IntArray? = null
    private var byKey: WordTable<IntArray>? = null

    // The clauses with no key, for the keys a frozen predicate does not have
    private var frozenMiss: IntArray? = null

    /** Whether a clause keys an argument beyond the first */
    val deep: Boolean get() = table.deep

    /** How many clauses there are */
    val size: Int get() = table.size

    fun add(
        clause: ClauseCode,
        onTop: Boolean,
    ) {
        val current = table
        val n = current.size
        val updated =
            if (current.pinned ||
                n == current.capacity
            ) {
                current.copied(maxOf(4, if (n == current.capacity) n * 2 else current.capacity))
            } else {
                current
            }
        val at = if (onTop) 0 else n
        if (onTop) {
            updated.clauses.copyInto(updated.clauses, 1, 0, n)
            updated.firstKeys.copyInto(updated.firstKeys, 1, 0, n)
            updated.starts.copyInto(updated.starts, 1, 0, n)
        }
        updated.clauses[at] = clause
        updated.firstKeys[at] = clause.key
        updated.starts[at] = clause.start
        updated.size = n + 1
        if (clause.deep) updated.deep = true
        if (clause.innerKeys.isNotEmpty()) updated.innerDeep = true
        table = updated
        changed()
    }

    fun removeAt(index: Int) {
        val current = table
        val n = current.size
        val updated = if (current.pinned) current.copied(current.capacity) else current
        updated.clauses.copyInto(updated.clauses, index, index + 1, n)
        updated.firstKeys.copyInto(updated.firstKeys, index, index + 1, n)
        updated.starts.copyInto(updated.starts, index, index + 1, n)
        updated.clauses[n - 1] = null
        updated.size = n - 1
        updated.deep = false
        updated.innerDeep = false
        for (i in 0 until n - 1) {
            val clause = updated.clauses[i]!!
            if (clause.deep) updated.deep = true
            if (clause.innerKeys.isNotEmpty()) updated.innerDeep = true
        }
        table = updated
        changed()
    }

    private fun changed() {
        all = null
        byKey = null
        deepIndexes = null
        deepAsks = 0
    }

    // The indexes of the arguments beyond the first, by the position of the argument: each made once goals keep
    // asking for it with the clauses unchanged (replaced as a whole, for the threads reading a frozen predicate)
    private var deepIndexes: Array<DeepIndex?>? = null
    private var deepAsks = 0

    /** The clauses a goal may match by the key of an argument beyond the first; no table: it would not help */
    private class DeepIndex(
        val byKey: WordTable<IntArray>?,
        val miss: IntArray,
    )

    /**
     * The indexes of the clauses a goal with [key] in its argument [position] (beyond the first) may match, in
     * order; null if no index of that argument says
     */
    fun candidatesAt(
        position: Int,
        key: Int,
    ): IntArray? {
        val index = deepIndexes?.get(position)
        if (index == null) {
            if (++deepAsks < DEEP_INDEX_AFTER) return null
            return locked(this) { deepIndex(position) }.let { made -> made.byKey?.let { it[key] ?: made.miss } }
        }
        val table = index.byKey ?: return null
        return table[key] ?: index.miss
    }

    private fun deepIndex(position: Int): DeepIndex {
        deepIndexes?.get(position)?.let { return it }
        val current = table
        val n = current.size
        val groups = HashMap<Int, ArrayList<Int>>()
        val miss = ArrayList<Int>()
        for (i in 0 until n) {
            val keys = current.clauses[i]!!.keys
            val key = if (position < keys.size) keys[position] else 0
            if (key == 0) miss.add(i) else groups.getOrPut(key) { ArrayList() }.add(i)
        }
        // (clauses with no key are candidates for every key: the lists they lengthen are bounded, or the scan is as good)
        val worthIt = groups.size >= 2 && (miss.size <= DEEP_INDEX_MISSES || miss.size * groups.size <= n * DEEP_INDEX_MISSES)
        val table = if (worthIt) WordTable<IntArray>() else null
        if (table != null) {
            for ((key, group) in groups) {
                table[key] = (if (miss.isEmpty()) group else (group + miss).sorted()).toIntArray()
            }
        }
        val index = DeepIndex(table, miss.toIntArray())
        val updated = deepIndexes?.copyOf() ?: arrayOfNulls<DeepIndex>(MAX_DEEP_POSITION + 1)
        updated[position] = index
        deepIndexes = updated
        return index
    }

    /**
     * The indexes of the clauses a goal whose first argument has [key] may match, in order: all of them for an
     * unbound argument or a small predicate, otherwise those with the same key or none
     */
    fun candidates(key: Int): IntArray {
        val current = table
        if (key == 0 || current.size < INDEXED_FROM) return all ?: IntArray(current.size) { it }.also { all = it }
        val miss = frozenMiss
        if (miss != null) return byKey!![key] ?: miss
        val index = byKey ?: WordTable<IntArray>().also { byKey = it }
        return index.getOrPut(key) {
            val matching = ArrayList<Int>()
            for (i in 0 until current.size) {
                val k = current.firstKeys[i]
                if (k == 0 || k == key) matching.add(i)
            }
            matching.toIntArray()
        }
    }

    /** Everything [candidates] may need computed now: the predicate is then only read, by any thread */
    fun freeze() {
        val current = table
        val keys = current.firstKeys
        val n = current.size
        current.pinned = true
        all = IntArray(n) { it }
        if (n < INDEXED_FROM) return
        val index = WordTable<IntArray>()
        for (i in 0 until n) {
            val key = keys[i]
            if (key != 0 && index[key] == null) {
                index[key] = (0 until n).filter { keys[it] == 0 || keys[it] == key }.toIntArray()
            }
        }
        byKey = index
        frozenMiss = (0 until n).filter { keys[it] == 0 }.toIntArray()
    }

    companion object {
        const val INDEXED_FROM = 8

        /** The last argument an index is made for */
        const val MAX_DEEP_POSITION = 3

        private const val DEEP_INDEX_AFTER = 16
        private const val DEEP_INDEX_MISSES = 8
    }
}
