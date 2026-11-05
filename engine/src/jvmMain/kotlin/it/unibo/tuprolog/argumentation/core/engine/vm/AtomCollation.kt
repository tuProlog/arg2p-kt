package it.unibo.tuprolog.argumentation.core.engine.vm

import java.text.CollationKey
import java.text.Collator
import java.util.concurrent.ConcurrentHashMap

private val collator: Collator by lazy { Collator.getInstance() }

// What the collator would compute for each comparison of a name, once: the keys compare as the collator does
private val keys = ConcurrentHashMap<String, CollationKey>()
private const val KEYS_KEPT = 1 shl 17

private fun keyOf(name: String): CollationKey {
    keys[name]?.let { return it }
    if (keys.size >= KEYS_KEPT) keys.clear()
    return collator.getCollationKey(name).also { keys[name] = it }
}

internal actual fun compareAtomNames(
    first: String,
    second: String,
): Int = keyOf(first).compareTo(keyOf(second))
