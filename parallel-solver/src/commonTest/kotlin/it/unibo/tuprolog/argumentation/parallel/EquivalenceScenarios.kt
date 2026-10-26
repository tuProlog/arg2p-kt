package it.unibo.tuprolog.argumentation.parallel

import it.unibo.tuprolog.argumentation.core.Arg2pSolverFactory
import it.unibo.tuprolog.argumentation.core.runtime.settings.FlagsBuilder
import it.unibo.tuprolog.argumentation.parallel.message.EvaluationResponse
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import kotlin.test.Test
import kotlin.test.assertEquals

/**
 * The labels of the arguments for a statement are the same in a distributed solver, its theory partitioned among
 * workers, and in the standard solver on the whole graph (grounded, no graph extensions): each subclass runs them on
 * a way of running the nodes
 */
abstract class EquivalenceScenarios {
    // The number of arguments for a statement labelled in, out and und
    data class Labels(
        val inArgs: Int,
        val outArgs: Int,
        val undArgs: Int,
    )

    /** The evaluation of the statements of [theory], by a distributed solver labelling as the standard one with [settings] */
    abstract fun distributed(
        theory: String,
        settings: FlagsBuilder,
    ): (String) -> EvaluationResponse

    private fun centralised(
        theory: String,
        statement: String,
        settings: FlagsBuilder,
    ): Labels {
        val solver = Arg2pSolverFactory.default(theory = theory, settings = settings.create())
        solver.solve(Struct.parse("buildLabelSetsSilent", solver.operators)).first()
        // the arguments whose claim unifies with the statement, as the workers look for them
        val count = { label: String ->
            solver
                .solve(Struct.parse("graphStore::argumentLabel($label, A), argument::claim(A, $statement)", solver.operators))
                .count { it.isYes }
        }
        return Labels(count("in"), count("out"), count("und"))
    }

    private fun assertSameLabels(
        theory: String,
        vararg statements: String,
    ) = assertSameLabels(FlagsBuilder(graphExtensions = emptyList()), theory, *statements)

    private fun assertSameLabels(
        settings: FlagsBuilder,
        theory: String,
        vararg statements: String,
    ) {
        val solve = distributed(theory, settings)
        statements.forEach { statement ->
            val response = solve(statement)
            // a statement with no argument is reported as one und, for no argument
            val actual =
                if (response.undArgs.size == 1 &&
                    response.undArgs
                        .first()
                        .queryChain
                        .isEmpty()
                ) {
                    Labels(0, 0, 0)
                } else {
                    Labels(response.inArgs.size, response.outArgs.size, response.undArgs.size)
                }
            assertEquals(centralised(theory, statement, settings), actual, "the arguments for $statement")
        }
    }

    @Test
    fun premisesAreNotUndercut() =
        assertSameLabels("r1 :=> b.\nr3 : c => d.\nr2 : b => c.\nr4 : d => e.\nr5 :=> undercut(r1).", "b", "c", "undercut(r1)")

    @Test
    fun undercutsOfTheRulesOfSubArguments() = assertSameLabels("r1 :=> b.\nr2 : b => c.\nr3 : c => d.\nr5 :=> undercut(r2).", "c", "d")

    @Test
    fun attacksOnSubArgumentsFromAnotherWorker() =
        assertSameLabels("r1 :=> b.\nr2 : b => c.\nr3 : c => d.\nr4 :=> -b.", "b", "c", "d", "-b")

    @Test
    fun rebutsOfSubArgumentsFromAnotherWorker() =
        assertSameLabels("r1 :=> a.\nr2 : a => b.\nr3 : b => c.\nr4 :=> e.\nr5 : e => -b.", "b", "c", "-b")

    @Test
    fun reinstatement() = assertSameLabels("r1 :=> a.\nr2 : a => b.\nr3 :=> c.\nr4 : c => -b.\nr5 :=> undercut(r4).", "b", "-b")

    @Test
    fun strictRulesAreNotUndercut() = assertSameLabels("r1 :-> a.\nr2 : a -> b.\nr3 :=> undercut(r2).\nr4 :=> -b.", "b", "-b")

    @Test
    fun severalAttackersFromTheSameWorker() =
        assertSameLabels("r1 :=> a.\nr2 : a => b.\nr3 :=> c.\nr4 : c => -b.\nr5 : c => undercut(r2).\nr6 :=> -c.", "b", "-b", "c")

    @Test
    fun severalArgumentsFromTheSameWorker() =
        assertSameLabels("r1 :=> a.\nr2 : a => b.\nr3 : a => b.\nr4 :=> c.\nr5 : c => b.\nr6 :=> -a.", "b", "a")

    @Test
    fun aStatementWithNoArgument() = assertSameLabels("r1 :=> a.", "z")

    // Full theories: the parts that are not rules are shared by every worker

    @Test
    fun plainProlog() =
        assertSameLabels(
            "prologStrictCompatibility.\nbird(tweety).\nflies(X) :- bird(X).\n-flies(X) :-\n    penguin(X). % r1\npenguin(tweety). % f1",
            "flies(tweety)",
            "-flies(tweety)",
            "penguin(tweety)",
        )

    @Test
    fun customConflicts() = assertSameLabels("conflict([a], [b]).\nf1 :=> a.\nf2 :=> b.\nr1 : b => c.", "a", "b", "c")

    @Test
    fun prologChecksOnSharedPredicates() =
        assertSameLabels("old(X) :- X > 65.\nr1 : age(X), prolog(old(X)) => senior(X).\nf1 :=> age(80).\nf2 :=> age(30).", "senior(X)")

    @Test
    fun metaRules() = assertSameLabels("metaRules.\nr1 : a => b.\nf1 :=> a.\nr2 : c => applicable(r1).\nf2 :=> c.\nf3 :=> -c.", "b", "c")

    @Test
    fun transpositions() = assertSameLabels("autoTransposition.\ns1 : a -> b.\nf1 :=> a.\nf2 :=> -b.", "a", "b", "-a", "-b")

    // The graph is labelled by the catalog as the standard solver does: every semantics, every graph extension

    @Test
    fun preferences() = assertSameLabels(FlagsBuilder(), "r1 : a => b.\nr2 : c => -b.\nf1 :=> a.\nf2 :=> c.\nsup(r1, r2).", "b", "-b")

    @Test
    fun anotherSemantics() =
        assertSameLabels(
            FlagsBuilder(graphExtensions = emptyList(), argumentLabellingMode = "stable"),
            "r1 :=> a.\nr2 : a => b.\nr3 :=> c.\nr4 : c => -a.\nr5 :=> undercut(r4).",
            "a",
            "b",
            "-a",
        )

    @Test
    fun defeasiblePreferences() =
        assertSameLabels(
            FlagsBuilder(graphExtensions = listOf("standardPref", "defeasiblePref")),
            "r1 : p => q.\nr2 : s => -q.\nf1 :=> p.\nf2 :=> s.\nrs : [] => sup(r1, r2).",
            "q",
            "-q",
        )

    @Test
    fun conflictsArgued() = assertSameLabels("metaConflicts.\nr0 : [] => conflict(a, b).\nf1 :=> a.\nf2 :=> b.", "a", "b")

    @Test
    fun burdenOfPersuasion() =
        assertSameLabels(
            FlagsBuilder(argumentLabellingMode = "bp_grounded", graphExtensions = listOf("standardPref", "bp")),
            "bp(guilty).\nr1 : evidence => guilty.\nr2 : alibi => -guilty.\nf1 :=> evidence.\nf2 :=> alibi.",
            "guilty",
            "-guilty",
        )
}
