package it.unibo.tuprolog.argumentation.core.model.mining

import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.unify.Unificator

/**
 * The items, looked up by the term of each ([key]). Mined terms are matched through unification, which amounts to
 * structural equality when both sides are ground: in that case a hash lookup replaces the scan of all the candidates
 * for every attack, support and label.
 */
internal class TermLookup<T>(
    items: List<T>,
    key: (T) -> Term,
) {
    private val entries = items.map { key(it) to it }

    private val index: Map<Term, T>? =
        if (entries.all { it.first.isGround }) {
            mutableMapOf<Term, T>().also { map -> entries.forEach { (term, item) -> map.getOrPut(term) { item } } }
        } else {
            null
        }

    /** The first item whose term matches [term] */
    fun firstOrNull(term: Term): T? =
        if (index != null && term.isGround) {
            index[term]
        } else {
            entries.firstOrNull { Unificator.default.match(it.first, term) }?.second
        }
}
