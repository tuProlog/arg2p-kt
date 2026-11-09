package it.unibo.tuprolog.argumentation.parallel.consumer

import it.unibo.tuprolog.argumentation.Flags
import it.unibo.tuprolog.argumentation.checked
import it.unibo.tuprolog.argumentation.core.Arg2pSolverFactory
import it.unibo.tuprolog.core.Atom
import it.unibo.tuprolog.core.Clause
import it.unibo.tuprolog.core.EmptyList
import it.unibo.tuprolog.core.Integer
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.Tuple
import it.unibo.tuprolog.core.Var
import it.unibo.tuprolog.solve.Solver
import it.unibo.tuprolog.theory.Theory
import it.unibo.tuprolog.unify.Unificator

/**
 * A theory split for the consumers building its graph together: each of them solves [shared] with one of [partitions]
 * ([theoryOf]), all as Prolog text in the language of arg2p
 */
data class Partitioning(
    val shared: String,
    val partitions: List<String>,
    /** The rules of the partitions are those the theory converts to: the consumers convert them no further */
    val converted: Boolean = true,
    /** The theory has the axioms of temporal, among the rules of the partitions: not added again */
    val temporal: Boolean = false,
    /**
     * For each partition, the predicates (as [TheoryPartitioner] names them: `p/1`, `-p/1`) of the conclusions of the
     * others its rules build on: the premises of its rules, but those only keyed rules and keyed facts conclude (each
     * solver builds those on its own facts); null for all of them
     */
    val interests: List<Set<String>?> = partitions.map { null },
) {
    /**
     * The theory of the consumer of the partition [index]: the conversions turned off if applied, whatever the profile
     * of the theory presets
     */
    fun theoryOf(index: Int) = "$shared\n${if (converted) CONVERTED else ""}${if (temporal) TEMPORAL else ""}${partitions[index]}"

    internal companion object {
        const val CONVERTED = "autoTransposition(off).\nprologStrictCompatibility(off).\n"

        // The axioms of temporal, among the rules converted, not added again: the arguments may conclude open
        // statements all the same (openConclusions, an internal flag)
        const val TEMPORAL = "temporal(off).\nopenConclusions.\n"
    }
}

/**
 * The partitions of a theory, for the consumers building its graph together: each consumer owns the rules of its partition, and
 * builds on the arguments of the others as well (see [it.unibo.tuprolog.argumentation.parallel.consumer.ClientSolver]). The
 * rules and premises that support one another (a premise of one unifies with the conclusion of another) are kept
 * together as far as the number of partitions allows, so that fewer arguments travel between consumers.
 *
 * The rules are those the theory converts to (see the module parser): with their identifiers, their transpositions
 * (autoTransposition), the Prolog clauses argued (prologStrictCompatibility) and the axioms of time (temporal), each
 * written back as a rule of arg2p.
 * Everything else in the theory is shared by every partition (flags, preferences, conflicts, burdens, the Prolog
 * predicates the rules check), but the flags of the conversion, already applied (the consumers are told to apply them
 * no further, whatever the profile). A theory with naturalTerms is left whole, in a single partition.
 */
object TheoryPartitioner {
    private val conversionFlags = setOf("autoTransposition", "prologStrictCompatibility", "temporal")

    /**
     * The partitions of [theory], converted with [flags] and the flags of the theory: the groups of rules that
     * support one another, or, given [parts], at most that many partitions. The data is split by its keys when the
     * theory has keys (see [keyed]): the rules keyed in every partition, the facts by the hash of their key, the rest
     * in groups as for a theory with none. The groups go into partitions of about the same number of rules, a group
     * larger than its share first split where it joins parts that support nothing of one another (see [joined]), so that
     * they are explored side by side; a chain of rules, or a cycle, is never split.
     */
    fun partition(
        theory: String,
        parts: Int? = null,
        flags: Flags = Flags.defaults().graphExtensions(),
    ): Partitioning {
        val solver = Arg2pSolverFactory.default(theory = theory, settings = flags.library())
        val clauses = solver.staticKb.clauses.toList()
        if (solver.holds("setting(naturalTerms)")) {
            return Partitioning("", listOf(theory), converted = false)
        }
        solver.solveOnce("context_reset")
        // a theory its profile refuses is refused here, as by the standard solver
        checked(solver.solveOnce("phases::checkProfile"))
        val rules = rules(solver)
        val supporters = supporters(rules, solver.holds("setting(metaRules)"))
        val split = if (parts != null && parts > 1) keyed(rules, supporters, parts) else null
        val groups =
            split?.partitions
                ?: components(rules.indices.toList(), supporters)
                    .let { if (parts == null) it else pack(balanced(it, supporters, rules.size, parts), parts) }
        val metaRules = solver.holds("setting(metaRules)")
        val keyedOnly = keyedOnly(rules, split)
        val interests = groups.map { group -> if (metaRules) null else interest(group, rules, keyedOnly, split?.keyedRules.orEmpty()) }
        val shared = clauses.filterNot { isRule(it) || isConversionFlag(it) }
        val sharedText = if (shared.isEmpty()) "" else Theory.of(shared).toString(asPrologText = true)
        // the axioms of temporal are among the rules converted: the consumers do not add them again
        val temporal = solver.holds("setting(temporal)")
        return Partitioning(
            sharedText,
            groups.map { group -> group.joinToString("\n") { "${rules[it].text}." } },
            temporal = temporal,
            interests = interests,
        )
    }

    // The predicates of the premises of the rules of [group], but those in [keyedOnly] for the [keyed] rules (a premise of a
    // keyed rule of one key is on the solver of its facts; the other rules build on those of every solver); null if a
    // premise could be any statement
    private fun interest(
        group: List<Int>,
        rules: List<Rule>,
        keyedOnly: Set<String>,
        keyed: Set<Int>,
    ): Set<String>? {
        val predicates = HashSet<String>()
        for (rule in group) {
            for (premise in rules[rule].premises) {
                val predicate = key(premise) ?: return null
                if (rule !in keyed || predicate !in keyedOnly) predicates += predicate
            }
        }
        return predicates
    }

    // The predicates only keyed rules and keyed facts conclude: the premises of a keyed rule of one key are on the solver
    // of its facts, which builds on them; a predicate another rule concludes may come from any solver
    private fun keyedOnly(
        rules: List<Rule>,
        split: Split?,
    ): Set<String> {
        if (split == null) return emptySet()
        val keyed = split.keyedRules + split.keyedFacts
        val byPredicate = rules.indices.groupBy { key(rules[it].conclusion) }
        if (null in byPredicate) return emptySet()
        return byPredicate
            .filterValues { producers -> producers.all { it in keyed } }
            .keys
            .filterNotNull()
            .toSet()
    }

    // The data split by its keys: the partitions, the rules keyed (in every partition), the facts placed by their keys
    private class Split(
        val partitions: List<List<Int>>,
        val keyedRules: Set<Int>,
        val keyedFacts: Set<Int>,
    )

    // A rule or a premise of the theory, as converted: its conclusion, its premises supported by arguments (none for a
    // premise) and its text
    private class Rule(
        val id: Term,
        val premises: List<Term>,
        val conclusion: Term,
        val text: String,
    )

    private fun rules(solver: Solver): List<Rule> {
        val strictness = "parser::strictness(Id, S)"
        // (the positions of the premises argued: findall would copy them, their variables no longer the conclusion's)
        val argued = "findall(I, (append(Before, [A|_], P), parser::premiseKind(A, statement), length(Before, I)), Argued)"
        val rules =
            solver.solutions("parser::rule(Id, P, [C]), $argued, $strictness").map {
                val premises = it.getValue("P").castToList().toList()
                val body = if (premises.isEmpty()) EmptyList() else Tuple.wrapIfNeeded(*premises.toTypedArray())
                val arrow = if (it.getValue("S") == Atom.of("strict")) "->" else "=>"
                // Id : Body => Conclusion, that is =>(:(Id, Body), Conclusion)
                val rule = Struct.of(arrow, Struct.of(":", it.getValue("Id"), body), it.getValue("C"))
                val positions =
                    it
                        .getValue("Argued")
                        .castToList()
                        .toList()
                        .map { position -> (position as Integer).intValue.toInt() }
                Rule(it.getValue("Id"), positions.map { position -> premises[position] }, it.getValue("C"), rule.toString())
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

    // The split by data (pivoting, after Seib and Lausen, PODS 1991): a key position for each predicate such that a rule
    // has one variable at the key of each of its statements (those its arguments rest on, and its conclusion), so that an
    // instance of the rule rests on facts with one key alone, and concludes for it. The rules keyed go in every partition,
    // the facts (premises, and rules with no premise, of ground conclusions) whose predicate has a key by the hash of the
    // constant there, and the other rules in groups by their supports (see components), packed. Where the placement is
    // off (a fact a rule of another partition rests on), the graph store gives each solver the arguments of the others
    // (see SharedGraph): the split only decides who builds what. None if the theory has no rule keyed, or too few facts.
    private fun keyed(
        rules: List<Rule>,
        supporters: Array<Set<Int>>,
        parts: Int,
    ): Split? {
        val keys = HashMap<String, Int>()
        val keyedRules = mutableListOf<Int>()
        // (the rules with more statements first: those constrain the keys the most)
        rules.indices
            .filter { rules[it].statements().any { statement -> !statement.isGround } }
            .sortedByDescending { rules[it].premises.size }
            .forEach { if (keyRule(rules[it], keys)) keyedRules += it }
        if (keyedRules.isEmpty()) return null
        val buckets = List(parts) { mutableListOf<Int>() }
        var facts = 0
        for (i in rules.indices) {
            val rule = rules[i]
            if (rule.premises.isNotEmpty() || !rule.conclusion.isGround) continue
            val (predicate, arguments) = predicateOf(rule.conclusion) ?: continue
            val key = keys[predicate] ?: continue
            buckets[((arguments[key].toString().hashCode() % parts) + parts) % parts] += i
            facts++
        }
        if (facts < 2 * parts) return null
        val placed = (keyedRules + buckets.flatten()).toSet()
        val rest = rules.indices.filter { it !in placed }
        val groups = if (rest.isEmpty()) emptyList() else pack(balanced(components(rest, supporters), supporters, rest.size, parts), parts)
        return Split(
            List(parts) { part -> keyedRules + buckets[part] + groups.getOrNull(part).orEmpty() },
            keyedRules.toSet(),
            placed - keyedRules.toSet(),
        )
    }

    // The statements of a rule: its conclusion and its premises supported by arguments
    private fun Rule.statements(): List<Term> = listOf(conclusion) + premises

    // Whether [rule] is keyed with [keys], the keys of the predicates it has that have none chosen for it
    private fun keyRule(
        rule: Rule,
        keys: MutableMap<String, Int>,
    ): Boolean {
        val statements = rule.statements().map { predicateOf(it) ?: return false }
        val variables =
            statements
                .first()
                .second
                .filterIsInstance<Var>()
                .distinct()
        for (variable in variables) {
            val chosen = HashMap<String, Int>()
            val holds =
                statements.all { (predicate, arguments) ->
                    val key =
                        keys[predicate] ?: chosen[predicate] ?: arguments.indexOf(variable).also { if (it >= 0) chosen[predicate] = it }
                    key >= 0 && key < arguments.size && arguments[key] == variable
                }
            if (holds) {
                keys += chosen
                return true
            }
        }
        return false
    }

    // The predicate of a statement, as key/1 names it (-p/1 for -p(X)), and its arguments; none for a statement with none
    private fun predicateOf(term: Term): Pair<String, List<Term>>? {
        val statement = if (term is Struct && term.functor == "-" && term.arity == 1) term[0] else term
        if (statement !is Struct || statement.arity == 0) return null
        return key(term)!! to statement.args
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
    // A flag of the conversion stated by its name alone (autoTransposition.): applied, the consumers turn it off
    private fun isConversionFlag(clause: Clause): Boolean = clause.isFact && clause.head.let { it is Atom && it.value in conversionFlags }

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
