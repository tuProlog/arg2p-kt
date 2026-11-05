package it.unibo.tuprolog.argumentation.core.model.mining

import it.unibo.tuprolog.argumentation.core.runtime.canonical
import it.unibo.tuprolog.core.Term

/**
 * The items, looked up by the term of each ([key]), as the graph store identifies arguments: the same term up to the
 * names of its variables. Unification would not do: an argument for an open statement, `holdsAt(f, T)`, unifies with
 * its instance for some values, `holdsAt(f, 7)`, and would take its label, attacks and supports.
 */
internal class TermLookup<T>(
    items: List<T>,
    key: (T) -> Term,
) {
    private val index: Map<Term, T> =
        mutableMapOf<Term, T>().also { map -> items.forEach { map.getOrPut(key(it).canonical()) { it } } }

    /** The first item whose term is [term], up to the names of its variables */
    fun firstOrNull(term: Term): T? = index[term.canonical()]
}
