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
) {
    private var unifierValue: Substitution.Unifier? = null

    val isEmpty: Boolean
        get() = size == 0

    /** Whether [variable] is bound */
    operator fun contains(variable: Var): Boolean = root != null && root.get(variable, variable.hashCode(), 0) != null

    /** The term [variable] is bound to, as bound (not resolved); null if it is not bound */
    operator fun get(variable: Var): Term? = root?.get(variable, variable.hashCode(), 0)

    /** The unifier of these bindings, as tuProlog reads it: built once, when first asked for */
    val unifier: Substitution.Unifier
        get() {
            unifierValue?.let { return it }
            val built = if (size == 0) Substitution.empty() else Substitution.unifier(toMap(resolve = true))
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
        path: ArrayList<Var>?,
    ): Term =
        when {
            term is Var -> {
                val bound = root!!.get(term, term.hashCode(), 0)
                when {
                    bound == null -> term
                    bound.isKnownGround() -> bound
                    path != null && term in path -> term
                    else -> {
                        val visiting = (path ?: ArrayList(4)).also { it.add(term) }
                        resolve(bound, visiting).also { visiting.removeAt(visiting.size - 1) }
                    }
                }
            }
            term is Struct && !term.isKnownGround() -> term.rebuilt { resolve(it, path) }
            else -> term
        }

    /** These bindings with [added], the bindings of the variables of a goal (none of them bound) */
    fun extended(added: Map<Var, Term>): Bindings {
        if (added.isEmpty()) return this
        var node = root
        var last = newest
        var count = size
        if (added is VarMap) {
            for (i in 0 until added.size) {
                val variable = added.keyAt(i)
                val term = added.valueAt(i)
                val put = Node.put(node, variable, variable.hashCode(), term, 0)
                if (put.added) count++
                node = put.node
                last = Entry(variable, term, last)
            }
        } else {
            for ((variable, term) in added) {
                val put = Node.put(node, variable, variable.hashCode(), term, 0)
                if (put.added) count++
                node = put.node
                last = Entry(variable, term, last)
            }
        }
        return Bindings(node, last, count)
    }

    /** The bindings of the variables [kept], their terms resolved (the bindings left out no longer apply to them) */
    fun filter(kept: (Var) -> Boolean): Bindings {
        val map = toMap(resolve = true)
        val iterator = map.entries.iterator()
        while (iterator.hasNext()) if (!kept(iterator.next().key)) iterator.remove()
        return of(map)
    }

    /**
     * The bindings a solution keeps, as tuProlog cleans a solution up (`retaining`): those of [variables], the ones
     * of the query, and those binding a variable to one of them; resolved, in their order. The solution is cleaned up
     * anyway: this spares building the unifier of all the bindings only for most of them to be filtered out.
     */
    fun retaining(variables: Set<Var>): Bindings {
        val map = toMap(resolve = true)
        val iterator = map.entries.iterator()
        while (iterator.hasNext()) {
            val (variable, term) = iterator.next()
            if (variable !in variables && !(term is Var && term in variables)) iterator.remove()
        }
        return of(map)
    }

    // The bindings in their order, each variable once (the last term, where it was bound again), as tuProlog's map
    private fun toMap(resolve: Boolean): LinkedHashMap<Var, Term> {
        val entries = ArrayList<Entry>(size)
        var entry = newest
        while (entry != null) {
            entries.add(entry)
            entry = entry.previous
        }
        val map = LinkedHashMap<Var, Term>(size * 2)
        for (i in entries.indices.reversed()) {
            val e = entries[i]
            map[e.variable] = if (resolve) resolve(e.term, null) else e.term
        }
        return map
    }

    override fun toString(): String = toMap(resolve = true).toString()

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
        ): Term? {
            val bit = 1 shl ((hash ushr shift) and MASK)
            if (bitmap and bit == 0) return null
            return when (val slot = slots[(bitmap and (bit - 1)).countOneBits()]) {
                is Binding -> if (slot.hash == hash && slot.variable == variable) slot.term else null
                is Node -> slot.get(variable, hash, shift + BITS)
                else -> (slot as Collisions).get(variable)
            }
        }

        companion object {
            const val BITS = 5
            const val MASK = (1 shl BITS) - 1
            const val MAX_SHIFT = 30

            fun put(
                node: Node?,
                variable: Var,
                hash: Int,
                term: Term,
                shift: Int,
            ): Put {
                val binding = Binding(hash, variable, term)
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
                            replaced = binding
                            added = false
                        } else {
                            replaced = merged(slot, binding, shift + BITS)
                            added = true
                        }
                    is Node -> {
                        val put = put(slot, variable, hash, term, shift + BITS)
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

    private class Binding(
        val hash: Int,
        val variable: Var,
        val term: Term,
    )

    // The bindings of variables whose hashes are the same, up to the last bits
    private class Collisions(
        private val bindings: List<Binding>,
    ) {
        fun get(variable: Var): Term? = bindings.firstOrNull { it.variable == variable }?.term

        fun with(binding: Binding): Collisions = Collisions(bindings.filter { it.variable != binding.variable } + binding)
    }

    companion object {
        /** No binding */
        val EMPTY = Bindings(null, null, 0)

        /** The bindings of [unifier], as tuProlog built it (its terms with every binding applied) */
        fun of(unifier: Map<Var, Term>): Bindings {
            val bindings = EMPTY.extended(unifier)
            if (unifier is Substitution.Unifier && bindings !== EMPTY) bindings.unifierValue = unifier
            return bindings
        }
    }
}
