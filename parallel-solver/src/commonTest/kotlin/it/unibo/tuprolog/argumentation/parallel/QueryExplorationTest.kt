package it.unibo.tuprolog.argumentation.parallel

import it.unibo.tuprolog.argumentation.Flags
import it.unibo.tuprolog.argumentation.GraphExtension
import it.unibo.tuprolog.argumentation.Semantics
import it.unibo.tuprolog.argumentation.core.Arg2pSolverFactory
import it.unibo.tuprolog.argumentation.core.model.mining.activeBranch
import it.unibo.tuprolog.argumentation.core.model.mining.arguments
import it.unibo.tuprolog.argumentation.parallel.cluster.LocalCluster
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.parsing.parse
import kotlin.test.Test
import kotlin.test.assertEquals

/**
 * Query mode explores the same arguments on a distributed solver as on the standard one: those the labels of the
 * arguments for the statement evaluated depend on, as the semantics declares, whatever the partitions of the theory
 */
class QueryExplorationTest {
    // The arguments the standard solver builds to answer [query]
    private fun local(
        theory: String,
        flags: Flags,
        query: String,
    ): Set<String> {
        val solver = Arg2pSolverFactory.default(theory = theory, settings = flags.library())
        solver.solve(Struct.parse("answerQuery(($query), In, Out, Und)", solver.operators)).first()
        return solver.arguments(solver.activeBranch()).map { written(it.termRepresentation()) }.toSet()
    }

    // The arguments of the graph a distributed solver of [parts] partitions labels to evaluate [query]
    private fun distributed(
        theory: String,
        flags: Flags,
        query: String,
        parts: Int,
    ): Set<String> {
        val cluster = LocalCluster(parts = parts, flags = flags).also { it.load(theory) }
        cluster.solve(query)
        return cluster.site.graphArguments
            .map { written(it) }
            .toSet()
    }

    // An argument as text, but for the identifiers of the graph store in the arguments derived from the others (the
    // copy of a premise for a conflict, mc(Id, Conflict), numbered as the arguments came)
    private fun written(argument: Term) = argument.toString().replace(Regex("""mc\(\d+,"""), "mc(_,")

    // grounded and complete are directional, naive connected
    private fun assertSameExploration(
        theory: String,
        vararg queries: String,
    ) = listOf(Semantics.GROUNDED, Semantics.COMPLETE, Semantics.NAIVE).forEach { semantics ->
        assertSameExploration(Flags.defaults().argumentLabellingMode(semantics), theory, *queries)
    }

    private fun assertSameExploration(
        flags: Flags,
        theory: String,
        vararg queries: String,
    ) = queries.forEach { query ->
        val expected = local(theory, flags, query)
        listOf(1, 2, 3).forEach { parts ->
            assertEquals(expected, distributed(theory, flags, query, parts), "$query, $flags, $parts partitions")
        }
    }

    @Test
    fun chainsOfRulesWithRebuttalsAndPreferences() =
        assertSameExploration(
            "r1 :=> a.\nr2 : a => b.\nr3 :=> c.\nr4 : c => d.\nr5 :=> -d.\nr6 : d => e.\nr7 :=> -a.\nsup(r1, r7).",
            "b",
            "e",
            "c",
        )

    @Test
    fun theInstancesOfARuleTheQueryNeedsAlone() =
        assertSameExploration(
            "r1 : p(X) => q(X).\nf1 :=> p(1).\nf2 :=> p(2).\nf3 :=> p(3).\nn1 :=> -q(2).\nr2 : q(2) => s.",
            "q(1)",
            "s",
        )

    @Test
    fun undercutsAndUnderminers() =
        assertSameExploration(
            "r1 :=> a.\nr2 : a => b.\nu1 :=> undercut(r2).\nr3 :=> -a.\nr4 : b => c.",
            "c",
            "b",
        )

    // the conflicts are arguments as well: those the labels depend on are explored through their hooks
    @Test
    fun conflictsArgued() =
        assertSameExploration("metaConflicts.\nr0 : [] => conflict(a, b).\nf1 :=> a.\nf2 :=> b.\nr1 : b => c.", "a", "b", "c")

    // the burdens of persuasion relate the arguments as their graph extension declares (connected)
    @Test
    fun burdensOfPersuasion() =
        assertSameExploration(
            Flags.defaults().argumentLabellingMode(Semantics.BP_GROUNDED).graphExtensions(GraphExtension.STANDARD_PREF, GraphExtension.BP),
            "bp(guilty).\nr1 : evidence => guilty.\nr2 : alibi => -guilty.\nf1 :=> evidence.\nf2 :=> alibi.\nr3 :=> other.",
            "guilty",
            "-guilty",
        )
}
