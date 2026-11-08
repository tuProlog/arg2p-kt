package it.unibo.tuprolog.argumentation.parallel.consumer

import it.unibo.tuprolog.argumentation.Flags
import it.unibo.tuprolog.argumentation.core.Arg2pSolverFactory
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.solve.Solver
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertTrue

class TheoryPartitionerTest {
    // The identifiers of the rules of each partition, sorted, the partitions sorted
    private fun groups(
        theory: String,
        parts: Int? = null,
    ): List<List<String>> =
        TheoryPartitioner
            .partition(theory, parts)
            .partitions
            .map { partition ->
                partition
                    .lines()
                    .map {
                        Regex("""\(':'\((\w+)|\((\w+),""").find(it)!!.groupValues.drop(1).first { id ->
                            id.isNotEmpty()
                        }
                    }.sorted()
            }.sortedBy { it.first() }

    @Test
    fun theRulesSupportingOneAnotherAreTogether() {
        // r3 needs c from r2, which needs b from r1; -b is apart
        assertEquals(listOf(listOf("r1", "r2", "r3"), listOf("r4")), groups("r1 :=> b.\nr2 : b => c.\nr3 : c => d.\nr4 :=> -b."))
    }

    @Test
    fun theGroupsArePackedWhole() {
        val theory = "r1 :=> b.\nr2 : b => c.\nr3 :=> x.\nr4 :=> y.\nr5 :=> z."
        assertEquals(listOf(listOf("r1", "r2", "r3"), listOf("r4", "r5")), groups(theory, parts = 2))
    }

    // Fewer groups than partitions: a chain stays whole (split in halves, it would only make a pipeline)
    @Test
    fun aChainIsNotSplit() =
        assertEquals(
            listOf(listOf("r1", "r2", "r3", "r4")),
            groups("r4 : c => d.\nr2 : a => b.\nr1 :=> a.\nr3 : b => c.", parts = 2),
        )

    // Rules with variables at one key of their statements: in every partition, the facts by their key, a fact and the
    // ones resting on it together (p0(K), e(K), and the conflicts on done(K))
    @Test
    fun theDataIsSplitByItsKeys() {
        val facts = (0 until 8).joinToString("\n") { "fp$it :=> p0($it).\nfe$it :=> e($it)." }
        val theory = "$facts\nr1(X) : p0(X) => p1(X).\nlast(X) : p1(X) => done(X).\nno(X) : e(X) => -done(X)."
        val partitions = groups(theory, parts = 4)
        assertEquals(4, partitions.size)
        partitions.forEach { assertTrue(it.containsAll(listOf("r1", "last", "no")), "$it") }
        // every fact once, a fact for p0(K) where the one for e(K) is
        val placed = partitions.flatMap { part -> part.filter { it.startsWith("f") } }
        assertEquals(16, placed.size)
        assertEquals(16, placed.toSet().size)
        for (k in 0 until 8) assertEquals(1, partitions.count { "fp$k" in it && "fe$k" in it }, "$k")
    }

    // The conclusions only keyed rules build are not of interest to the keyed rules (those of one key are on the solver of
    // its facts), but they are to a rule with no key: it builds on those of every solver
    @Test
    fun aRuleWithNoKeyIsInterestedInTheKeyedConclusions() {
        val facts = (0 until 8).joinToString("\n") { "fp$it :=> p($it)." }
        val partitioning = TheoryPartitioner.partition("$facts\nr(X) : p(X) => s(X).\nt : s(X) => u.", 4, Flags.defaults())
        val withT =
            partitioning.partitions.indices.single {
                "t : s" in partitioning.partitions[it] ||
                    ":'(t," in partitioning.partitions[it]
            }
        partitioning.partitions.indices.forEach { part ->
            val interest = partitioning.interests[part]
            if (part == withT) assertTrue(interest == null || "s/1" in interest, "$interest") else assertEquals(emptySet(), interest)
        }
    }

    // A rule with no variable at one key of all its statements (a join of two keys) is not keyed: in a group, not in
    // every partition
    @Test
    fun aJoinIsNotKeyed() {
        val facts = (0 until 8).joinToString("\n") { "fa$it :=> a($it).\nfb$it :=> b($it)." }
        val partitions = groups("$facts\nj(X, Y) : a(X), b(Y) => c(X, Y).\nk(X) : a(X) => d(X).", parts = 4)
        assertEquals(4, partitions.count { "k" in it })
        assertEquals(1, partitions.count { "j" in it })
    }

    // A group joining parts that support nothing of one another (g on the chains of b and of d) is split there, each
    // part whole: they are explored side by side
    @Test
    fun theGroupsAreSplitWhereTheyJoinIndependentParts() =
        assertEquals(
            listOf(listOf("a1", "a2"), listOf("c1", "c2", "g")),
            groups("a1 :=> a.\na2 : a => b.\nc1 :=> c.\nc2 : c => d.\ng : b, d => e.", parts = 2),
        )

    @Test
    fun whatIsNotARuleIsShared() {
        val partitioning =
            TheoryPartitioner.partition("graphExtension(standardPref).\nsup(r1, r2).\nconflict([a], [b]).\nold(X) :- X > 65.\nr1 :=> a.")
        listOf("graphExtension", "sup", "conflict", "old").forEach { assertTrue(it in partitioning.shared, partitioning.shared) }
        assertTrue(":=>" !in partitioning.shared, partitioning.shared)
    }

    // The Prolog clauses argued are rules with the identifiers of their comments, or of their positions; the flag of
    // the conversion is not shared, as the clauses are converted already
    @Test
    fun prologClausesBecomeRules() {
        val partitioning = TheoryPartitioner.partition("prologStrictCompatibility.\nbird(tweety).\nflies(X) :- bird(X). % r1")
        val rules = partitioning.partitions.joinToString("\n")
        assertTrue("':=>'(r1" !in rules && "r1" in rules && "':->'(rule_" in rules, rules)
        assertTrue("prologStrictCompatibility" !in partitioning.shared, partitioning.shared)
    }

    @Test
    fun transpositionsAreRules() {
        val partitioning = TheoryPartitioner.partition("autoTransposition.\ns1 : a -> b.\nf1 :=> a.")
        val rules = partitioning.partitions.joinToString("\n")
        assertTrue("s1_i" in rules, rules)
        assertTrue("autoTransposition" !in partitioning.shared, partitioning.shared)
    }

    // With metaRules a rule needs an argument for applicable(Id): the rules concluding it are with it
    @Test
    fun metaRulesSupportTheirRules() =
        assertEquals(listOf(listOf("f1", "r1", "r2")), groups("metaRules.\nr1 : a => b.\nf1 :=> a.\nr2 : [] => applicable(r1)."))

    @Test
    fun aTheoryWithNoRuleHasNoPartition() = assertEquals(emptyList(), TheoryPartitioner.partition("sup(r1, r2).").partitions)

    // The axioms of temporal are rules of the theory converted, partitioned as the others: a consumer does not add them
    // again, its arguments concluding open statements all the same
    @Test
    fun theAxiomsOfTimeArePartitionedOnce() {
        val theory = "temporal.\ne1 :=> happens(load, 1).\ni1 :=> initiates(load, loaded).\nq1 : holdsAt(loaded, 3) => canShoot."
        val partitioning = TheoryPartitioner.partition(theory)
        assertTrue(partitioning.temporal)
        val members =
            partitioning.partitions.indices.map { index ->
                Arg2pSolverFactory
                    .default(
                        theory = partitioning.theoryOf(index),
                        settings = Flags.parse("temporal.").library(),
                    ).also {
                        it.solve(Struct.parse("context_reset, parser:::convertAllRules(_)", it.operators)).first()
                    }
            }
        val axioms = { solver: Solver ->
            solver.solve(Struct.parse("parser::rule(t_after(_, _), _, _)", solver.operators)).count { it.isYes }
        }
        assertEquals(1, members.sumOf(axioms))
        members.forEach { assertTrue(it.solve(Struct.parse("parser::instances", it.operators)).first().isYes) }
        partitioning.partitions.indices.forEach { assertTrue(partitioning.theoryOf(it).contains("openConclusions")) }
    }
}
