package it.unibo.tuprolog.argumentation.core.engine.fsm

import it.unibo.tuprolog.argumentation.core.runtime.isKnownGround
import it.unibo.tuprolog.argumentation.core.runtime.rebuilt
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Substitution
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.Var

/**
 * The substitution of a resolution (arg2p): the bindings of the variables of its goals, kept as found and read
 * through each other. tuProlog keeps a substitution as a map whose values have every binding applied, so that each
 * step binding a variable applied its bindings to all the values and copied the map twice: a step cost as much as
 * all the variables bound before it. Here a binding is added to a persistent map (a hash trie, shared by the frames
 * and the choice points: a step copies the path to the binding alone), and the term bound to a variable is resolved
 * when read, through the variables bound after it ([resolved]); the unifier tuProlog reads ([unifier]) is built when
 * first asked for, once, by a solution, or by the primitives that read the substitution of their context.
 *
 * The bindings are the same as the unifier built the eager way: a binding never binds a variable bound before
 * (the goals have the substitution applied, so the variables they bind are not bound), and a term resolved through
 * the bindings after it is the term with each of them applied in turn. A variable bound to a term holding it (there
 * is no occurs check) is left in it, as the eager way left it after applying the binding once.
 */
class Bindings private constructor(
    private val root: Node?,
    // The bindings in the order they were added, the newest first (the order of the unifier, as tuProlog kept it)
    private val newest: Entry?,
    /** The number of variables bound */
    val size: Int,
    // How many bindings bound a variable to a variable (see retaining)
    private val aliases: Int,
) {
    private var unifierValue: Substitution.Unifier? = null

    val isEmpty: Boolean
        get() = size == 0

    /** Whether [variable] is bound */
    operator fun contains(variable: Var): Boolean = root != null && root.get(variable, variable.hashCode(), 0) != null

    /** The term [variable] is bound to, as bound (not resolved); null if it is not bound */
    operator fun get(variable: Var): Term? = root?.get(variable, variable.hashCode(), 0)?.term

    /** The unifier of these bindings, as tuProlog reads it: built once, when first asked for */
    val unifier: Substitution.Unifier
        get() {
            unifierValue?.let { return it }
            val built = if (size == 0) Substitution.empty() else Substitution.unifier(toMap(Resolver()))
            unifierValue = built
            return built
        }

    /**
     * [term] with the bindings applied, in a single pass: the parts ground at hand are kept (see [isKnownGround]), the
     * others rebuilt where a variable is bound, to its term resolved in turn
     */
    fun resolved(term: Term): Term = if (root == null) term else resolve(term, null)

    // [path]: the variables being resolved, a variable bound to a term holding it left in it
    private fun resolve(
        term: Term,
        path: Path?,
    ): Term =
        when {
            term is Var -> {
                val bound = root!!.get(term, term.hashCode(), 0)?.term
                when {
                    bound == null -> term
                    bound.isKnownGround() -> bound
                    path != null && term in path -> term
                    else -> {
                        val visiting = (path ?: Path()).also { it.push(term) }
                        resolve(bound, visiting).also { visiting.pop() }
                    }
                }
            }
            term is Struct && !term.isKnownGround() -> term.rebuilt { resolve(it, path) }
            else -> term
        }

    // The variables being resolved: a list, indexed once it is long (a list built by binding its tails in turn is
    // resolved through all of them, each looked up in the path)
    private class Path {
        private val variables = ArrayList<Var>(4)
        private var indexed: HashSet<Var>? = null

        operator fun contains(variable: Var): Boolean = indexed?.contains(variable) ?: variables.contains(variable)

        fun push(variable: Var) {
            variables.add(variable)
            val index = indexed
            when {
                index != null -> index.add(variable)
                variables.size > INDEXED_FROM -> indexed = HashSet(variables)
            }
        }

        fun pop() {
            val variable = variables.removeAt(variables.size - 1)
            indexed?.remove(variable)
        }

        private companion object {
            const val INDEXED_FROM = 8
        }
    }

    /** These bindings with [added], the bindings of the variables of a goal (none of them bound) */
    fun extended(added: Map<Var, Term>): Bindings {
        if (added.isEmpty()) return this
        var node = root
        var last = newest
        var count = size
        var aliased = aliases
        if (added is VarMap) {
            for (i in 0 until added.size) {
                val variable = added.keyAt(i)
                val term = added.valueAt(i)
                val put = Node.put(node, variable, variable.hashCode(), term, count, 0)
                if (put.added) count++
                if (term is Var) aliased++
                node = put.node
                last = Entry(variable, term, last)
            }
        } else {
            for ((variable, term) in added) {
                val put = Node.put(node, variable, variable.hashCode(), term, count, 0)
                if (put.added) count++
                if (term is Var) aliased++
                node = put.node
                last = Entry(variable, term, last)
            }
        }
        return Bindings(node, last, count, aliased)
    }

    /**
     * The bindings of the variables [kept], their terms resolved (the bindings left out no longer apply to them):
     * the terms of the bindings left out are not resolved (resolving every binding of a frame left, to keep a few,
     * cost as much as all the bindings of the resolution)
     */
    fun filter(kept: (Var) -> Boolean): Bindings = of(toMap(Resolver()) { variable, _ -> kept(variable) })

    /**
     * The bindings a solution keeps, as tuProlog cleans a solution up (`retaining`): those of [variables], the ones
     * of the query, and those binding a variable to one of them; resolved, in their order. The solution is cleaned up
     * anyway: this spares building the unifier of all the bindings only for most of them to be filtered out.
     */
    fun retaining(variables: Set<Var>): Bindings {
        val resolver = Resolver()
        if (aliases == 0) {
            // No variable bound to a variable: the bindings of the variables given alone, looked up (the solutions
            // of a sub-resolution, as many as the bindings of a long recursion, each walked all of them)
            val root = root ?: return EMPTY
            val found = ArrayList<Binding>(variables.size)
            for (variable in variables) root.get(variable, variable.hashCode(), 0)?.let { found.add(it) }
            if (found.isEmpty()) return EMPTY
            found.sortBy { it.order }
            val map = LinkedHashMap<Var, Term>(found.size * 2)
            for (binding in found) map[binding.variable] = resolver.resolve(binding.term)
            return of(map)
        }
        return of(
            toMap(resolver) { variable, term ->
                // (a term that is not a variable never resolves to one: only the variables are resolved to tell)
                variable in variables || (term is Var && resolver.resolve(term).let { it is Var && it in variables })
            },
        )
    }

    // The bindings in their order, each variable once (the last term, where it was bound again), as tuProlog's map:
    // those [kept] (told by the variable and its term as bound), their terms resolved by [resolver]
    private fun toMap(
        resolver: Resolver,
        kept: ((Var, Term) -> Boolean)? = null,
    ): LinkedHashMap<Var, Term> {
        val entries = ArrayList<Entry>(if (kept == null) size else KEPT_CAPACITY)
        var entry = newest
        while (entry != null) {
            if (kept == null || kept(entry.variable, entry.term)) entries.add(entry)
            entry = entry.previous
        }
        val map = LinkedHashMap<Var, Term>(entries.size * 2)
        for (i in entries.indices.reversed()) {
            val e = entries[i]
            map[e.variable] = resolver.resolve(e.term)
        }
        return map
    }

    override fun toString(): String = toMap(Resolver()).toString()

    /**
     * The resolution of the terms of many bindings at once (the unifier, the bindings kept by a frame left or by a
     * solution): each variable is resolved once ([resolve] alone resolved the chain of the bindings a term refers
     * to, binding by binding, for each binding of the chain: a list built by binding its tails in turn cost the
     * square of its length). A variable met again while its own term is resolved (a term holding the variable, see
     * [resolve]) is left in it, as [resolve] leaves it, and the terms resolved through it, which depend on where
     * their resolution started, are not kept.
     */
    private inner class Resolver {
        // The term each variable resolved to, or RESOLVING while its term is being resolved
        private val memo = HashMap<Var, Any>()
        private var cycles = 0

        fun resolve(term: Term): Term {
            if (term is Var) {
                val bound = root?.get(term, term.hashCode(), 0)?.term ?: return term
                if (bound.isKnownGround()) return bound
                when (val known = memo[term]) {
                    null -> Unit
                    RESOLVING -> {
                        cycles++
                        return term
                    }
                    else -> return known as Term
                }
                memo[term] = RESOLVING
                val before = cycles
                val resolved = resolve(bound)
                if (cycles == before) memo[term] = resolved else memo.remove(term)
                return resolved
            }
            return if (term is Struct && !term.isKnownGround()) term.rebuilt { resolve(it) } else term
        }
    }

    // A binding in the order the bindings were added
    private class Entry(
        val variable: Var,
        val term: Term,
        val previous: Entry?,
    )

    // The result of adding a binding to a node: the node, and whether the variable was not bound before
    private class Put(
        val node: Node,
        val added: Boolean,
    )

    // A node of the hash trie: a bitmap of the slots in use, for five bits of the hash at a time, each slot holding a
    // binding, a node of the next five bits, or the bindings of variables whose hashes are the same
    private class Node(
        private val bitmap: Int,
        private val slots: Array<Any>,
    ) {
        fun get(
            variable: Var,
            hash: Int,
            shift: Int,
        ): Binding? {
            val bit = 1 shl ((hash ushr shift) and MASK)
            if (bitmap and bit == 0) return null
            return when (val slot = slots[(bitmap and (bit - 1)).countOneBits()]) {
                is Binding -> if (slot.hash == hash && slot.variable == variable) slot else null
                is Node -> slot.get(variable, hash, shift + BITS)
                else -> (slot as Collisions).get(variable)
            }
        }

        companion object {
            const val BITS = 5
            const val MASK = (1 shl BITS) - 1
            const val MAX_SHIFT = 30

            // [order]: the place of the binding among the others, kept where the variable is bound again
            fun put(
                node: Node?,
                variable: Var,
                hash: Int,
                term: Term,
                order: Int,
                shift: Int,
            ): Put {
                val binding = Binding(hash, variable, term, order)
                if (node == null) return Put(Node(1 shl ((hash ushr shift) and MASK), arrayOf(binding)), true)
                val bit = 1 shl ((hash ushr shift) and MASK)
                val position = (node.bitmap and (bit - 1)).countOneBits()
                if (node.bitmap and bit == 0) {
                    val slots = arrayOfNulls<Any>(node.slots.size + 1)
                    node.slots.copyInto(slots, 0, 0, position)
                    slots[position] = binding
                    node.slots.copyInto(slots, position + 1, position, node.slots.size)
                    @Suppress("UNCHECKED_CAST")
                    return Put(Node(node.bitmap or bit, slots as Array<Any>), true)
                }
                val slot = node.slots[position]
                val replaced: Any
                val added: Boolean
                when (slot) {
                    is Binding ->
                        if (slot.hash == hash && slot.variable == variable) {
                            replaced = Binding(hash, variable, term, slot.order)
                            added = false
                        } else {
                            replaced = merged(slot, binding, shift + BITS)
                            added = true
                        }
                    is Node -> {
                        val put = put(slot, variable, hash, term, order, shift + BITS)
                        replaced = put.node
                        added = put.added
                    }
                    else -> {
                        val collisions = slot as Collisions
                        added = collisions.get(variable) == null
                        replaced = collisions.with(binding)
                    }
                }
                val slots = node.slots.copyOf()
                slots[position] = replaced
                return Put(Node(node.bitmap, slots), added)
            }

            // The slot holding two bindings of different variables at [shift]: a node telling them by the next bits
            // of their hashes, or their collisions once the hashes are exhausted
            private fun merged(
                first: Binding,
                second: Binding,
                shift: Int,
            ): Any {
                if (shift > MAX_SHIFT) return Collisions(listOf(first, second))
                val firstIndex = (first.hash ushr shift) and MASK
                val secondIndex = (second.hash ushr shift) and MASK
                val bits = (1 shl firstIndex) or (1 shl secondIndex)
                // (the slots in the order of their indexes: the bit of the last index is negative)
                return when {
                    firstIndex == secondIndex -> Node(bits, arrayOf(merged(first, second, shift + BITS)))
                    firstIndex < secondIndex -> Node(bits, arrayOf(first, second))
                    else -> Node(bits, arrayOf(second, first))
                }
            }
        }
    }

    // A binding, with its place among the others ([order]: the bindings of a variable bound again keep the first)
    private class Binding(
        val hash: Int,
        val variable: Var,
        val term: Term,
        val order: Int,
    )

    // The bindings of variables whose hashes are the same, up to the last bits
    private class Collisions(
        private val bindings: List<Binding>,
    ) {
        fun get(variable: Var): Binding? = bindings.firstOrNull { it.variable == variable }

        fun with(binding: Binding): Collisions {
            val known = get(binding.variable)
            val kept = if (known == null) binding else Binding(binding.hash, binding.variable, binding.term, known.order)
            return Collisions(bindings.filter { it.variable != binding.variable } + kept)
        }
    }

    companion object {
        /** No binding */
        val EMPTY = Bindings(null, null, 0, 0)

        // The mark of a variable whose term is being resolved (see Resolver)
        private val RESOLVING = Any()

        // The bindings a frame left or a solution keeps, as a rule: a few
        private const val KEPT_CAPACITY = 8

        /** The bindings of [unifier], as tuProlog built it (its terms with every binding applied) */
        fun of(unifier: Map<Var, Term>): Bindings {
            val bindings = EMPTY.extended(unifier)
            if (unifier is Substitution.Unifier && bindings !== EMPTY) bindings.unifierValue = unifier
            return bindings
        }
    }
}
