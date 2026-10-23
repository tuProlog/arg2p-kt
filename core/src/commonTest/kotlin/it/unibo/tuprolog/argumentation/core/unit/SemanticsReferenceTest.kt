package it.unibo.tuprolog.argumentation.core.unit

import it.unibo.tuprolog.argumentation.core.TestingUtils.prepareContext
import it.unibo.tuprolog.argumentation.core.TestingUtils.solver
import it.unibo.tuprolog.argumentation.core.dsl.arg2pScope
import it.unibo.tuprolog.argumentation.core.model.Argument
import it.unibo.tuprolog.argumentation.core.model.Attack
import it.unibo.tuprolog.argumentation.core.model.Graph
import it.unibo.tuprolog.argumentation.core.model.LabelledArgument
import it.unibo.tuprolog.argumentation.core.model.mining.graph
import kotlin.random.Random
import kotlin.test.Test
import kotlin.test.assertTrue

/**
 * Brute-force reference implementation of the abstract semantics, straight from their definitions
 * (Baroni, Caminada, Giacomin, "An introduction to argumentation semantics", KER 2011;
 * Baroni et al. 2005 for the SCC-recursive cf2/stage2; Caminada 2014 for strong admissibility).
 * Sets of arguments are bitmasks, labellings are the Ext2Lab image of the extensions.
 */
private class ReferenceSemantics(
    val n: Int,
    val att: Set<Pair<Int, Int>>,
) {
    private val all = (1 shl n) - 1
    private val subsets = (0..all).toList()

    private fun members(s: Int) = (0 until n).filter { s and (1 shl it) != 0 }

    private fun subset(
        a: Int,
        b: Int,
    ) = a and b.inv() == 0

    fun cf(s: Int) = members(s).none { a -> members(s).any { b -> (a to b) in att } }

    fun plus(s: Int) = (0 until n).filter { b -> members(s).any { a -> (a to b) in att } }.fold(0) { m, b -> m or (1 shl b) }

    private fun defends(
        s: Int,
        a: Int,
    ) = (0 until n).filter { (it to a) in att }.all { b -> plus(s) and (1 shl b) != 0 }

    private fun f(s: Int) = (0 until n).filter { defends(s, it) }.fold(0) { m, a -> m or (1 shl a) }

    private fun maximal(sets: List<Int>) = sets.filter { s -> sets.none { it != s && subset(s, it) } }

    private fun maximalBy(
        sets: List<Int>,
        key: (Int) -> Int,
    ) = sets.filter { s -> sets.none { key(it) != key(s) && subset(key(s), key(it)) } }

    private fun range(s: Int) = s or plus(s)

    val conflictFree by lazy { subsets.filter { cf(it) } }
    val admissible by lazy { conflictFree.filter { subset(it, f(it)) } }
    val complete by lazy { conflictFree.filter { it == f(it) } }
    val grounded by lazy {
        var s = 0
        while (f(s) != s) s = f(s)
        listOf(s)
    }
    val preferred by lazy { maximal(admissible) }
    val stable by lazy { conflictFree.filter { range(it) == all } }
    val semistable by lazy { maximalBy(complete, ::range) }
    val ideal by lazy { biggestAdmissibleIn(preferred) }
    val eager by lazy { biggestAdmissibleIn(semistable) }
    val naive by lazy { maximal(conflictFree) }
    val stage by lazy { maximalBy(conflictFree, ::range) }

    private fun biggestAdmissibleIn(exts: List<Int>): List<Int> {
        val inter = exts.fold(all) { m, e -> m and e }
        return maximal(admissible.filter { subset(it, inter) })
    }

    private val saMemo = mutableMapOf<Int, Boolean>()

    private fun stronglyAdmissible(s: Int): Boolean =
        saMemo.getOrPut(s) {
            cf(s) &&
                members(s).all { a ->
                    val rest = s and (1 shl a).inv()
                    subsets.any { t -> subset(t, rest) && stronglyAdmissible(t) && defends(t, a) }
                }
        }

    val stronglyAdmissibleSets by lazy { subsets.filter { stronglyAdmissible(it) } }

    // SCC-recursive scheme (Baroni et al. 2005), base semantics applied to single-SCC restrictions
    private fun reach(args: Int): Array<BooleanArray> {
        val r = Array(n) { i -> BooleanArray(n) { j -> i == j } }
        members(args).forEach { a -> members(args).forEach { b -> if ((a to b) in att) r[a][b] = true } }
        for (k in 0 until n) for (i in 0 until n) for (j in 0 until n) if (r[i][k] && r[k][j]) r[i][j] = true
        return r
    }

    private fun sccs(args: Int): List<Int> {
        val r = reach(args)
        return members(args).map { a -> members(args).filter { r[a][it] && r[it][a] }.fold(0) { m, b -> m or (1 shl b) } }.distinct()
    }

    private fun restricted(args: Int) =
        ReferenceSemantics(n, att.filter { (a, b) -> args and (1 shl a) != 0 && args and (1 shl b) != 0 }.toSet())

    private fun sccRecursive(
        args: Int,
        e: Int,
        base: (ReferenceSemantics, Int) -> List<Int>,
    ): Boolean {
        val components = sccs(args)
        if (components.size <= 1) return e in base(restricted(args), args)
        return components.all { s ->
            val outside = e and args and s.inv()
            val up = members(s).filter { a -> members(outside).none { (it to a) in att } }.fold(0) { m, a -> m or (1 shl a) }
            sccRecursive(up, e and s, base)
        }
    }

    // base semantics on the restriction to args (sets outside args are discarded)
    private fun naiveOn(
        r: ReferenceSemantics,
        args: Int,
    ) = r.maximal(r.conflictFree.filter { r.subset(it, args) })

    private fun stageOn(
        r: ReferenceSemantics,
        args: Int,
    ) = r.maximalBy(r.conflictFree.filter { r.subset(it, args) }, r::range)

    val cf2 by lazy { conflictFree.filter { sccRecursive(all, it, ::naiveOn) } }
    val stage2 by lazy { conflictFree.filter { sccRecursive(all, it, ::stageOn) } }

    fun ext2lab(s: Int): String {
        val out = plus(s)
        return (0 until n).joinToString(" ") { a ->
            when {
                s and (1 shl a) != 0 -> "in"
                out and (1 shl a) != 0 -> "out"
                else -> "und"
            } + "a$a"
        }
    }

    fun expected(semantics: String): List<String> =
        when (semantics) {
            "conflictfree" -> conflictFree
            "admissible" -> admissible
            "stronglyadmissible" -> stronglyAdmissibleSets
            "complete" -> complete
            "grounded", "grounded_old" -> grounded
            "preferred" -> preferred
            "stable" -> stable
            "semistable" -> semistable
            "ideal" -> ideal
            "eager" -> eager
            "naive" -> naive
            "stage" -> stage
            "cf2" -> cf2
            "stage2" -> stage2
            else -> error(semantics)
        }.map { ext2lab(it) }.sorted()
}

class SemanticsReferenceTest {
    private val semantics =
        listOf(
            "conflictfree",
            "admissible",
            "stronglyadmissible",
            "complete",
            "grounded",
            "grounded_old",
            "preferred",
            "stable",
            "semistable",
            "ideal",
            "eager",
            "naive",
            "stage",
            "cf2",
            "stage2",
        )

    private fun actual(
        n: Int,
        att: Set<Pair<Int, Int>>,
        sem: String,
    ): List<String> {
        val args = (0 until n).map { Argument(listOf("ra$it"), "ra$it", "a$it") }
        return arg2pScope {
            solver().let { solver ->
                prepareContext(
                    solver,
                    Graph.of(args.map { LabelledArgument(it, "und") }, att.map { (a, b) -> Attack(args[a], args[b]) }, emptyList()),
                )
                solver
                    .solve(sem call "argumentLabelling")
                    .filter { it.isYes }
                    .map { _ ->
                        val byArg = solver.graph().labellings.groupBy { it.argument.topRule.removePrefix("r") }
                        (0 until n).joinToString(" ") { a ->
                            (byArg["a$a"]?.joinToString("+") { it.label } ?: "MISSING") + "a$a"
                        }
                    }.toList()
                    .sorted()
            }
        }
    }

    private fun check(frameworks: List<Pair<Int, Set<Pair<Int, Int>>>>) {
        val failures =
            frameworks.flatMap { (n, att) ->
                val reference = ReferenceSemantics(n, att)
                semantics.mapNotNull { sem ->
                    val expected = reference.expected(sem)
                    val actual = actual(n, att, sem)
                    if (expected == actual) {
                        null
                    } else {
                        "$sem on n=$n att=${att.map { (a, b) -> "a$a->a$b" }}: expected $expected but was $actual"
                    }
                }
            }
        assertTrue(failures.isEmpty(), failures.joinToString("\n"))
    }

    // Arguments defending each other without being rooted in unattacked arguments are not strongly admissible
    @Test
    fun knownCases() =
        check(
            listOf(
                3 to setOf(0 to 1, 1 to 0, 1 to 2, 2 to 1),
                4 to setOf(0 to 2, 2 to 1, 1 to 3, 3 to 0),
                4 to setOf(0 to 3, 2 to 3, 3 to 2),
            ),
        )

    @Test
    fun randomFrameworks() {
        val random = Random(42)
        check(
            (1..20).map {
                val n = 2 + random.nextInt(3)
                n to
                    (0 until n)
                        .flatMap { a -> (0 until n).map { b -> a to b } }
                        .filter { (a, b) -> random.nextDouble() < (if (a == b) 0.1 else 0.3) }
                        .toSet()
            },
        )
    }
}
