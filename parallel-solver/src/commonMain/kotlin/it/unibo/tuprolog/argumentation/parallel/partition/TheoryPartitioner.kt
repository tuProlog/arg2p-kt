package it.unibo.tuprolog.argumentation.parallel.partition

import it.unibo.tuprolog.argumentation.Flags
import it.unibo.tuprolog.argumentation.core.Arg2pSolverFactory
import it.unibo.tuprolog.argumentation.parallel.node.checked
import it.unibo.tuprolog.argumentation.parallel.node.holds
import it.unibo.tuprolog.argumentation.parallel.node.solutions
import it.unibo.tuprolog.argumentation.parallel.node.solveOnce
import it.unibo.tuprolog.core.Atom
import it.unibo.tuprolog.core.Clause
import it.unibo.tuprolog.core.EmptyList
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.Tuple
import it.unibo.tuprolog.core.Var
import it.unibo.tuprolog.solve.Solver
import it.unibo.tuprolog.theory.Theory
import it.unibo.tuprolog.unify.Unificator

/**
 * A theory split for the workers of a distributed solver: each of them solves [shared] with one of [partitions]
 * ([theoryOf]), all as Prolog text in the language of arg2p
 */
data class Partitioning(
    val shared: String,
    val partitions: List<String>,
    /** The rules of the partitions are those the theory converts to: the workers convert them no further */
    val converted: Boolean = true,
    /** The theory of the catalog: its shared part, or the whole theory when it is not split */
    val catalog: String = shared,
) {
    /**
     * The theory of the worker of the partition [index]: the conversions turned off if applied, whatever the profile
     * of the theory presets
     */
    fun theoryOf(index: Int) = "$shared\n${if (converted) CONVERTED else ""}${partitions[index]}"

    private companion object {
        const val CONVERTED = "autoTransposition(off).\nprologStrictCompatibility(off).\n"
    }
}

/**
 * The partitions of a theory, for the workers of a distributed solver: each worker owns the rules of its partition, and
 * builds on the arguments of the others as well (see [it.unibo.tuprolog.argumentation.parallel.node.Catalog]). The
 * rules and premises that support one another (a premise of one unifies with the conclusion of another) are kept
 * together as far as the number of partitions allows, so that fewer arguments travel between workers.
 *
 * The rules are those the theory converts to (see the module parser): with their identifiers, their transpositions
 * (autoTransposition), and the Prolog clauses argued (prologStrictCompatibility), each written back as a rule of arg2p.
 * Everything else in the theory is shared by every partition (flags, preferences, conflicts, burdens, the Prolog
 * predicates the rules check), but the flags of the conversion, already applied (the workers are told to apply them
 * no further, whatever the profile). A theory with naturalTerms is left whole, in a single partition.
 */
object TheoryPartitioner {
    private val conversionFlags = setOf("autoTransposition", "prologStrictCompatibility")

    /**
     * The partitions of [theory], converted with [flags] and the flags of the theory: the groups of rules that
     * support one another, or, given [parts], that many partitions of about the same number of rules. A group larger
     * than its share is first split where it joins parts that support nothing of one another (see [joined]), so that
     * they are explored side by side; the groups are then packed together, and the largest partitions split in halves
     * if there are still fewer than [parts].
     */
    fun partition(
        theory: String,
        parts: Int? = null,
        flags: Flags = Flags.defaults().graphExtensions(),
    ): Partitioning {
        val solver = Arg2pSolverFactory.default(theory = theory, settings = flags.library())
        val clauses = solver.staticKb.clauses.toList()
        if (clauses.any { it.isFact && it.head == Atom.of("naturalTerms") }) {
            return Partitioning("", listOf(theory), converted = false, catalog = theory)
        }
        solver.solveOnce("context_reset, parser:::convertAllRules(_)")
        // a theory its profile refuses is refused here, as by the standard solver
        checked(solver.solveOnce("structuredWhole::checkProfile"))
        val rules = rules(solver)
        val supporters = supporters(rules, solver.holds("setting(metaRules)"))
        val groups =
            components(rules.indices.toList(), supporters)
                .let { if (parts == null) it else split(pack(balanced(it, supporters, rules.size, parts), parts), parts) }
        val shared = clauses.filterNot { isRule(it) || (it.isFact && it.head.let { h -> h is Atom && h.value in conversionFlags }) }
        return Partitioning(
            if (shared.isEmpty()) "" else Theory.of(shared).toString(asPrologText = true),
            groups.map { group -> group.joinToString("\n") { "${rules[it].text}." } },
        )
    }

    // A rule or a premise of the theory, as converted: its conclusion, its premises supported by arguments (none for a
    // premise) and its text
    private class Rule(
        val id: Term,
        val premises: List<Term>,
        val conclusion: Term,
        val text: String,
    )

    private fun rules(solver: Solver): List<Rule> {
        val strictness = "(parser::checkStrict(Id) -> S = strict ; S = defeasible)"
        val rules =
            solver.solutions("parser::rule(Id, P, [C]), findall(A, (member(A, P), parser::argued(A)), Argued), $strictness").map {
                val premises = it.getValue("P").castToList().toList()
                val body = if (premises.isEmpty()) EmptyList() else Tuple.wrapIfNeeded(*premises.toTypedArray())
                val arrow = if (it.getValue("S") == Atom.of("strict")) "->" else "=>"
                // Id : Body => Conclusion, that is =>(:(Id, Body), Conclusion)
                val rule = Struct.of(arrow, Struct.of(":", it.getValue("Id"), body), it.getValue("C"))
                Rule(it.getValue("Id"), it.getValue("Argued").castToList().toList(), it.getValue("C"), rule.toString())
            }
        val premises =
            solver.solutions("parser::premise(Id, [C]), $strictness").map {
                val arrow = if (it.getValue("S") == Atom.of("strict")) ":->" else ":=>"
                Rule(it.getValue("Id"), emptyList(), it.getValue("C"), Struct.of(arrow, it.getValue("Id"), it.getValue("C")).toString())
            }
        return rules + premises
    }

    // The rules supporting each rule (indexes in rules): those whose conclusion unifies with one of its premises; with
    // metaRules, a rule has applicable(Id) for premise as well
    private fun supporters(
        rules: List<Rule>,
        metaRules: Boolean,
    ): Array<Set<Int>> {
        val byKey = rules.indices.groupBy { key(rules[it].conclusion) }
        val unificator = Unificator.default
        return Array(rules.size) { index ->
            val rule = rules[index]
            val premises = rule.premises + (if (metaRules) listOf(Struct.of("applicable", rule.id)) else emptyList())
            premises
                .flatMap { premise ->
                    val key = key(premise)
                    val candidates = if (key == null) rules.indices.toList() else byKey[key].orEmpty() + byKey[null].orEmpty()
                    candidates.filter { unificator.match(rules[it].conclusion, premise) }
                }.toSet()
        }
    }

    // The groups of [rules] (indexes) supporting one another, as [supporters] relates them, each in the order of its
    // supports
    private fun components(
        rules: List<Int>,
        supporters: Array<Set<Int>>,
    ): List<List<Int>> {
        val members = rules.toSet()
        val parent = rules.associateWith { it }.toMutableMap()

        fun find(i: Int): Int {
            var root = i
            while (parent.getValue(root) != root) root = parent.getValue(root)
            var node = i
            while (parent.getValue(node) != root) {
                val next = parent.getValue(node)
                parent[node] = root
                node = next
            }
            return root
        }
        rules.forEach { rule -> supporters[rule].filter { it in members }.forEach { parent[find(it)] = find(rule) } }
        return rules
            .groupBy { find(it) }
            .values
            .map { ordered(it, supporters) }
    }

    // The groups larger than their share of the [total] rules for [parts] partitions split where they join parts that
    // support nothing of one another (see [joined]), the largest first, as long as they can be
    private fun balanced(
        groups: List<List<Int>>,
        supporters: Array<Set<Int>>,
        total: Int,
        parts: Int,
    ): List<List<Int>> {
        val share = (total + parts - 1) / parts
        val result = groups.toMutableList()
        val whole = mutableSetOf<List<Int>>()
        while (true) {
            val largest = result.filter { it.size > share && it !in whole }.maxByOrNull { it.size } ?: return result
            val pieces = joined(largest, supporters)
            if (pieces.size < 2) {
                whole += largest
            } else {
                result.remove(largest)
                result += pieces
            }
        }
    }

    // A group split where it joins parts that support nothing of one another: its last rules, those no rule of the
    // group builds on (as one that joins chains), are set apart, then those of what is left, until what is left falls
    // apart; the parts, and the rules set apart together. A group that never falls apart (a chain, a cycle) stays whole.
    private fun joined(
        group: List<Int>,
        supporters: Array<Set<Int>>,
    ): List<List<Int>> {
        var rest = group.toSet()
        val apart = mutableListOf<Int>()
        while (rest.isNotEmpty()) {
            val used = rest.flatMap { rule -> supporters[rule].filter { it in rest && it != rule } }.toSet()
            val last = rest.filter { it !in used }
            if (last.isEmpty()) return listOf(group)
            apart += last
            rest = rest - last.toSet()
            val parts = components(group.filter { it in rest }, supporters)
            if (parts.size > 1) return parts + listOf(ordered(apart, supporters))
        }
        return listOf(group)
    }

    // The rules of a group in the order of their supports, each after those supporting it (those in a cycle of
    // supports last, in the order of the theory): a group split there cuts few supports, a chain only one
    private fun ordered(
        group: List<Int>,
        supporters: Array<Set<Int>>,
    ): List<Int> {
        val members = group.toSet()
        val inside = group.associateWith { rule -> supporters[rule].filter { it in members && it != rule } }
        val pending = inside.mapValues { it.value.size }.toMutableMap()
        val supported = inside.entries.flatMap { (rule, supports) -> supports.map { it to rule } }.groupBy({ it.first }, { it.second })
        val queue = ArrayDeque(group.filter { pending.getValue(it) == 0 })
        val order = mutableListOf<Int>()
        while (queue.isNotEmpty()) {
            val rule = queue.removeFirst()
            order += rule
            supported[rule].orEmpty().forEach {
                pending[it] = pending.getValue(it) - 1
                if (pending.getValue(it) == 0) queue.addLast(it)
            }
        }
        val placed = order.toSet()
        return order + group.filter { it !in placed }
    }

    // The groups packed into at most parts partitions, the largest first into the smallest partition
    private fun pack(
        groups: List<List<Int>>,
        parts: Int,
    ): List<List<Int>> {
        val bins = List(maxOf(1, minOf(parts, groups.size))) { mutableListOf<Int>() }
        groups.sortedByDescending { it.size }.forEach { group -> bins.minBy { it.size }.addAll(group) }
        return bins.filter { it.isNotEmpty() }
    }

    // The largest partition split in halves (its rules in the order of their supports), until there are parts of them
    private fun split(
        partitions: List<List<Int>>,
        parts: Int,
    ): List<List<Int>> {
        val result = partitions.toMutableList()
        while (result.isNotEmpty() && result.size < parts) {
            val largest = result.maxBy { it.size }
            if (largest.size < 2) break
            result.remove(largest)
            result += largest.subList(0, largest.size / 2)
            result += largest.subList(largest.size / 2, largest.size)
        }
        return result
    }

    // The index of a statement: its functor and arity (inside a strong negation), none for a variable
    private fun key(term: Term): String? =
        when {
            term is Var -> null
            term is Struct && term.functor == "-" && term.arity == 1 -> key(term[0])?.let { "-$it" }
            term is Struct -> "${term.functor}/${term.arity}"
            else -> term.toString()
        }

    // A rule or a premise written in the language of arg2p (Id : Body => Conclusion parses as =>(:(Id, Body),
    // Conclusion)): it is converted, the rules of the partitions replace it
    private fun isRule(clause: Clause): Boolean {
        if (!clause.isFact) return false
        val head = clause.head ?: return false
        return when {
            head.arity != 2 -> false
            head.functor == ":=>" || head.functor == ":->" -> true
            head.functor == "=>" || head.functor == "->" -> head[0].let { it is Struct && it.functor == ":" && it.arity == 2 }
            else -> false
        }
    }
}
