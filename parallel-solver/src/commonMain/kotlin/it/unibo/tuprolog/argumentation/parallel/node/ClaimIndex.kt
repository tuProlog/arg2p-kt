package it.unibo.tuprolog.argumentation.parallel.node

import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.Var
import it.unibo.tuprolog.unify.Unificator

/**
 * [items] by the statement each is about ([statementOf]), for those unifying with a statement with no scan of them all: by
 * the functor of the statement (of what it negates, for a negation) and its first argument, if a constant. What unifies is
 * what a scan with [Unificator.match] would find, in the order of [items].
 */
internal class ClaimIndex<T>(
    items: List<T> = emptyList(),
    private val statementOf: (T) -> Term,
    private val unificator: Unificator = Unificator.default,
) {
    private val items = ArrayList<T>(items.size)
    private val statements = ArrayList<Term>(items.size)

    // The positions of the items: of those whose statement is a variable, and of the others by functor
    private val anything = ArrayList<Int>()
    private val byFunctor = HashMap<String, Bucket>()

    private class Bucket {
        val all = ArrayList<Int>()
        val open = ArrayList<Int>()
        val byConstant = HashMap<Term, MutableList<Int>>()
    }

    init {
        items.forEach { add(it) }
    }

    /** [item] added, after the others */
    fun add(item: T) {
        val position = items.size
        items += item
        val statement = statementOf(item)
        statements += statement
        val struct = core(statement)
        if (struct == null) {
            anything += position
        } else {
            val bucket = byFunctor.getOrPut(key(struct, statement)) { Bucket() }
            bucket.all += position
            val constant = constantOf(struct)
            if (constant == null) bucket.open += position else bucket.byConstant.getOrPut(constant) { ArrayList() } += position
        }
    }

    /** No item left */
    fun clear() {
        items.clear()
        statements.clear()
        anything.clear()
        byFunctor.clear()
    }

    /** The items whose statements unify with [statement], in their order */
    fun matching(statement: Term): List<T> = positions(statement).map { items[it] }

    /** Whether the statement of an item unifies with [statement] */
    fun anyMatching(statement: Term): Boolean = positions(statement, first = true).isNotEmpty()

    /** The items whose statements unify with one of [patterns], in their order, each once */
    fun matchingAny(patterns: Iterable<Term>): List<T> {
        val found = HashSet<Int>()
        patterns.forEach { found += positions(it) }
        return found.sorted().map { items[it] }
    }

    private fun positions(
        statement: Term,
        first: Boolean = false,
    ): List<Int> {
        val found = ArrayList<Int>()
        for (position in candidates(statement)) {
            if (unificator.match(statement, statements[position])) {
                found += position
                if (first) break
            }
        }
        return found
    }

    private fun candidates(statement: Term): List<Int> {
        val struct = core(statement) ?: return items.indices.toList()
        val bucket = byFunctor[key(struct, statement)]
        val constant = constantOf(struct)
        val inBucket =
            when {
                bucket == null -> emptyList()
                constant == null -> bucket.all
                else -> bucket.open + bucket.byConstant[constant].orEmpty()
            }
        return if (anything.isEmpty()) inBucket else inBucket + anything
    }

    private companion object {
        // What a statement is about: itself, or what it negates; null for what may unify across the keys (a variable, a negated one)
        fun core(statement: Term): Struct? =
            when {
                statement is Var -> null
                statement is Struct && statement.arity == 1 && statement.functor == "-" -> statement[0] as? Struct
                statement is Struct -> statement
                else -> null
            }

        fun key(
            core: Struct,
            statement: Term,
        ) = (if (core === statement) "" else "-") + core.functor + "/" + core.arity

        // The first argument, if an atom or an integer: a compound one, or a variable, may unify with others, and reals with others
        // written otherwise
        fun constantOf(core: Struct): Term? = if (core.arity == 0) null else core[0].takeIf { it.isAtom || it.isInteger }
    }
}
