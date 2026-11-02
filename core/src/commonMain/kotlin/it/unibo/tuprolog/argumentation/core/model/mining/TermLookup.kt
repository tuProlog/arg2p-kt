package it.unibo.tuprolog.argumentation.core.model.mining

import it.unibo.tuprolog.core.Integer
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Substitution
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
        mutableMapOf<Term, T>().also { map -> items.forEach { map.getOrPut(canonical(key(it))) { it } } }

    /** The first item whose term is [term], up to the names of its variables */
    fun firstOrNull(term: Term): T? = index[canonical(term)]

    // The term with its variables numbered in the order they appear: the same for the terms equal up to their names
    private fun canonical(term: Term): Term =
        if (term.isGround) {
            term
        } else {
            term.apply(
                Substitution.of(
                    term.variables
                        .distinct()
                        .mapIndexed {
                            i,
                            variable,
                            ->
                            variable to Struct.of("\$VAR", Integer.of(i))
                        }.toMap(),
                ),
            )
        }
}
