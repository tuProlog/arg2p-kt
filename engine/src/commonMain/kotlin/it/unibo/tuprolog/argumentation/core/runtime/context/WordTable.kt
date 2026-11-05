package it.unibo.tuprolog.argumentation.core.runtime.context

/**
 * A table from words of the machine (not 0) to values: open addressing, no boxing, a copy is a copy of two arrays. The
 * words of a kind share their low bits, which a hash table taking the low bits of the keys sends all to a few bins.
 */
internal class WordTable<V : Any> private constructor(
    private var keys: IntArray,
    private var values: Array<Any?>,
    size: Int,
) {
    constructor() : this(IntArray(8), arrayOfNulls(8), 0)

    /** How many keys */
    var size: Int = size
        private set

    private fun slot(key: Int): Int {
        val mask = keys.size - 1
        var at = (key * -1640531527).let { it xor (it ushr 15) } and mask
        while (keys[at] != 0 && keys[at] != key) at = (at + 1) and mask
        return at
    }

    @Suppress("UNCHECKED_CAST")
    operator fun get(key: Int): V? = values[slot(key)] as V?

    operator fun set(
        key: Int,
        value: V,
    ) {
        var at = slot(key)
        if (keys[at] == 0) {
            if ((size + 1) * 4 > keys.size * 3) {
                grow()
                at = slot(key)
            }
            keys[at] = key
            size++
        }
        values[at] = value
    }

    private fun grow() {
        val oldKeys = keys
        val oldValues = values
        keys = IntArray(oldKeys.size * 2)
        values = arrayOfNulls(oldKeys.size * 2)
        for (i in oldKeys.indices) {
            if (oldKeys[i] != 0) {
                val at = slot(oldKeys[i])
                keys[at] = oldKeys[i]
                values[at] = oldValues[i]
            }
        }
    }

    inline fun getOrPut(
        key: Int,
        make: () -> V,
    ): V = get(key) ?: make().also { set(key, it) }

    /** Every value, in no order */
    @Suppress("UNCHECKED_CAST")
    fun values(): List<V> = values.filterNotNull() as List<V>

    fun copy(): WordTable<V> = WordTable(keys.copyOf(), values.copyOf(), size)
}
