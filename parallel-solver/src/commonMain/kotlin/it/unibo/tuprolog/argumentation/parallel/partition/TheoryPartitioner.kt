package it.unibo.tuprolog.argumentation.parallel.partition

import it.unibo.tuprolog.argumentation.Flags
import it.unibo.tuprolog.argumentation.core.Arg2pSolverFactory
import it.unibo.tuprolog.core.Atom
import it.unibo.tuprolog.core.Clause
import it.unibo.tuprolog.core.EmptyList
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.Tuple
import it.unibo.tuprolog.core.Var
import it.unibo.tuprolog.core.parsing.parse
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
     * support one another, or, given [parts], that many partitions of about the same number of rules (the groups
     * packed together, or the largest ones split in halves). Given [only], the rules (and premises) with those
     * identifiers alone, as text (see [QueryScope][it.unibo.tuprolog.argumentation.parallel.node.QueryScope]).
     */
    fun partition(
        theory: String,
        parts: Int? = null,
        flags: Flags = Flags.defaults().graphExtensions(),
        only: Set<String>? = null,
    ): Partitioning {
        val solver = Arg2pSolverFactory.default(theory = theory, settings = flags.library())
        val clauses = solver.staticKb.clauses.toList()
        if (clauses.any { it.isFact && it.head == Atom.of("naturalTerms") }) return Partitioning("", listOf(theory), converted = false)
        solver.solveOnce("context_reset, parser:::convertAllRules(_)")
        val rules = rules(solver).filter { only == null || it.id.toString() in only }
        val groups = groups(rules, solver.holds("setting(metaRules)")).let { if (parts == null) it else split(pack(it, parts), parts) }
        val shared = clauses.filterNot { isRule(it) || (it.isFact && it.head.let { h -> h is Atom && h.value in conversionFlags }) }
        return Partitioning(
            if (shared.isEmpty()) "" else Theory.of(shared).toString(asPrologText = true),
            groups.map { group -> group.joinToString("\n") { "${rules[it].text}." } },
        )
    }

    // A rule or a premise of the theory, as converted: its conclusion, its premises (none for a premise) and its text
    private class Rule(
        val id: Term,
        val premises: List<Term>,
        val conclusion: Term,
        val text: String,
    )

    private fun rules(solver: Solver): List<Rule> {
        val strictness = "(parser::checkStrict(Id) -> S = strict ; S = defeasible)"
        val rules =
            solver.solutions("parser::rule(Id, P, [C]), $strictness").map {
                val premises = it.getValue("P").castToList().toList()
                val body = if (premises.isEmpty()) EmptyList() else Tuple.wrapIfNeeded(*premises.toTypedArray())
                val arrow = if (it.getValue("S") == Atom.of("strict")) "->" else "=>"
                // Id : Body => Conclusion, that is =>(:(Id, Body), Conclusion)
                val rule = Struct.of(arrow, Struct.of(":", it.getValue("Id"), body), it.getValue("C"))
                Rule(it.getValue("Id"), premises, it.getValue("C"), rule.toString())
            }
        val premises =
            solver.solutions("parser::premise(Id, [C]), $strictness").map {
                val arrow = if (it.getValue("S") == Atom.of("strict")) ":->" else ":=>"
                Rule(it.getValue("Id"), emptyList(), it.getValue("C"), Struct.of(arrow, it.getValue("Id"), it.getValue("C")).toString())
            }
        return rules + premises
    }

    // The groups of rules supporting one another (indexes in rules), by the union of those whose conclusion unifies
    // with a premise of another; with metaRules, a rule has applicable(Id) for premise as well
    private fun groups(
        rules: List<Rule>,
        metaRules: Boolean,
    ): List<List<Int>> {
        val parent = IntArray(rules.size) { it }

        fun find(i: Int): Int {
            var root = i
            while (parent[root] != root) root = parent[root]
            var node = i
            while (parent[node] != root) {
                val next = parent[node]
                parent[node] = root
                node = next
            }
            return root
        }
        val byKey = rules.indices.groupBy { key(rules[it].conclusion) }
        val unificator = Unificator.default
        val supporters = Array(rules.size) { mutableSetOf<Int>() }
        rules.forEachIndexed { index, rule ->
            val premises = rule.premises + (if (metaRules) listOf(Struct.of("applicable", rule.id)) else emptyList())
            premises.filterNot { isCheck(it) }.forEach { premise ->
                val key = key(premise)
                val candidates = if (key == null) rules.indices.toList() else byKey[key].orEmpty() + byKey[null].orEmpty()
                candidates
                    .filter { unificator.match(rules[it].conclusion, premise) }
                    .forEach {
                        supporters[index] += it
                        parent[find(it)] = find(index)
                    }
            }
        }
        return rules.indices
            .groupBy { find(it) }
            .values
            .map { ordered(it, supporters) }
    }

    // The rules of a group in the order of their supports, each after those supporting it (those in a cycle of
    // supports last, in the order of the theory): a group split there cuts few supports, a chain only one
    private fun ordered(
        group: List<Int>,
        supporters: Array<MutableSet<Int>>,
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

    // A premise that no argument supports: a contrary (~) or a Prolog check
    private fun isCheck(premise: Term) = premise is Struct && premise.arity == 1 && (premise.functor == "~" || premise.functor == "prolog")

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

    private fun Solver.solveOnce(goal: String) = solve(Struct.parse(goal, operators)).first()

    private fun Solver.holds(goal: String) = solveOnce(goal).isYes

    // The bindings of the variables of goal in each of its solutions
    private fun Solver.solutions(goal: String): List<Map<String, Term>> =
        solve(Struct.parse(goal, operators))
            .filter { it.isYes }
            .map { solution -> solution.substitution.entries.associate { (variable, value) -> variable.name to value } }
            .toList()
}
