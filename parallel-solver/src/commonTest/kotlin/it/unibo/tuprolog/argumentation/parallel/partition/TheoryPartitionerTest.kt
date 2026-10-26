package it.unibo.tuprolog.argumentation.parallel.partition

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

    // Fewer groups than partitions: the largest group is split in the order of its supports, a chain only once
    @Test
    fun theGroupsAreSplitForMorePartitions() =
        assertEquals(
            listOf(listOf("r1", "r2"), listOf("r3", "r4")),
            groups("r4 : c => d.\nr2 : a => b.\nr1 :=> a.\nr3 : b => c.", parts = 2),
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
}
