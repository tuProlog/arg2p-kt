package it.unibo.tuprolog.argumentation.parallel

import it.unibo.tuprolog.argumentation.Flags
import it.unibo.tuprolog.argumentation.GraphExtension
import it.unibo.tuprolog.argumentation.Semantics
import it.unibo.tuprolog.argumentation.core.Arg2pSolverFactory
import it.unibo.tuprolog.argumentation.parallel.message.EvaluationResponse
import it.unibo.tuprolog.argumentation.parallel.message.Response
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertTrue

/**
 * The statements answering a statement are labelled the same in a distributed solver, its theory partitioned among
 * workers, and in the standard solver answering it with the same flags (answerQuery/4, on the part of the graph the
 * statement depends on in query mode): each subclass runs them on a way of running the nodes
 */
abstract class EquivalenceScenarios {
    // The statements labelled in, out and und, as text, their variables named in the order they appear
    data class Labels(
        val inStatements: Set<String>,
        val outStatements: Set<String>,
        val undStatements: Set<String>,
    )

    /** The evaluation of the statements of [theory], by a distributed solver labelling as the standard one with [flags] */
    abstract fun distributed(
        theory: String,
        flags: Flags,
    ): (String) -> EvaluationResponse

    private fun named(statement: String): String {
        val names = mutableMapOf<String, String>()
        return Regex("""\b(_[A-Za-z0-9_]*|[A-Z][A-Za-z0-9_]*)\b""").replace(statement) { names.getOrPut(it.value) { "V${names.size}" } }
    }

    private fun centralised(
        theory: String,
        statement: String,
        flags: Flags,
    ): Labels {
        val solver = Arg2pSolverFactory.default(theory = theory, settings = flags.library())
        val solution = solver.solve(Struct.parse("answerQuery(($statement), In, Out, Und)", solver.operators)).first()
        val labelled = { name: String ->
            solution.substitution
                .getByName(name)!!
                .castToList()
                .toList()
                .map { named(it.toString()) }
                .toSet()
        }
        return Labels(labelled("In"), labelled("Out"), labelled("Und"))
    }

    private fun assertSameLabels(
        theory: String,
        vararg statements: String,
    ) = assertSameLabels(Flags.defaults().graphExtensions(), theory, *statements)

    private fun assertSameLabels(
        flags: Flags,
        theory: String,
        vararg statements: String,
    ) {
        val solve = distributed(theory, flags)
        statements.forEach { statement ->
            val response = solve(statement)
            // a statement with no argument is reported as one und, for no argument; every other with its arguments
            val none =
                response.undArgs.size == 1 &&
                    response.undArgs
                        .first()
                        .queryChain
                        .isEmpty()
            val labelled = { responses: List<Response> -> if (none) emptySet() else responses.map { named(it.claim) }.toSet() }
            val actual = Labels(labelled(response.inArgs), labelled(response.outArgs), labelled(response.undArgs))
            assertEquals(centralised(theory, statement, flags), actual, "the statements answering $statement")
            if (!none) {
                (response.inArgs + response.outArgs + response.undArgs).forEach {
                    assertTrue(it.queryChain.isNotEmpty(), "the arguments for ${it.claim}")
                }
            }
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
    fun preferences() = assertSameLabels(Flags.defaults(), "r1 : a => b.\nr2 : c => -b.\nf1 :=> a.\nf2 :=> c.\nsup(r1, r2).", "b", "-b")

    @Test
    fun anotherSemantics() =
        assertSameLabels(
            Flags.defaults().graphExtensions().argumentLabellingMode(Semantics.STABLE),
            "r1 :=> a.\nr2 : a => b.\nr3 :=> c.\nr4 : c => -a.\nr5 :=> undercut(r4).",
            "a",
            "b",
            "-a",
        )

    @Test
    fun defeasiblePreferences() =
        assertSameLabels(
            Flags.defaults().graphExtensions(GraphExtension.DEFEASIBLE_PREF),
            "r1 : p => q.\nr2 : s => -q.\nf1 :=> p.\nf2 :=> s.\nrs : [] => sup(r1, r2).",
            "q",
            "-q",
        )

    @Test
    fun conflictsArgued() = assertSameLabels("metaConflicts.\nr0 : [] => conflict(a, b).\nf1 :=> a.\nf2 :=> b.", "a", "b")

    // a graph not built from rules: by the catalog, as the standard solver builds it
    @Test
    fun abstractFrameworks() = assertSameLabels("graphBuildMode(abstract).\nd.\na -> b.\nb -> c.\n", "a", "b", "c", "d")

    // a theory not split: the catalog has all of it, its preferences included
    @Test
    fun naturalTerms() = assertSameLabels("naturalTerms.\nr1 : [] => a.\nr2 : [] => -a.\nsup(r1, r2).", "a", "-a")

    // the flags the theory states alone: those of its profile preset, in the workers as in the catalog
    @Test
    fun theFlagsOfTheTheoryAlone() =
        assertSameLabels(Flags.none(), "graphBuildMode(meta).\nr1 : [] => a.\nr2 : [] => -a.\nsup(r1, r2).", "a", "-a")

    @Test
    fun burdenOfPersuasion() =
        assertSameLabels(
            Flags.defaults().argumentLabellingMode(Semantics.BP_GROUNDED).graphExtensions(GraphExtension.STANDARD_PREF, GraphExtension.BP),
            "bp(guilty).\nr1 : evidence => guilty.\nr2 : alibi => -guilty.\nf1 :=> evidence.\nf2 :=> alibi.",
            "guilty",
            "-guilty",
        )

    // The instances of the arguments for sets of values, which the catalog adds as the standard solver does: those the
    // workers' arguments rest on, and those a conflict binds; on the whole graph, and in query mode (each statement on a
    // graph of its own, in both solvers)
    private fun bothModes(
        flags: Flags,
        theory: String,
        vararg statements: String,
    ) {
        assertSameLabels(flags.queryMode(false), theory, *statements)
        assertSameLabels(flags.queryMode(true), theory, *statements)
    }

    @Test
    fun constraints() =
        bothModes(
            Flags.defaults().graphExtensions(GraphExtension.STANDARD_PREF, GraphExtension.CONSTRAINTS),
            "f1 :=> s.\nr0 : s, {T > 1} => holdsAt(f, T).\nr1 : holdsAt(f, 5) => ok.\nc1 :=> -holdsAt(f, 7).\nc2 :=> -holdsAt(f, 0).",
            "ok",
            "holdsAt(f, 7)",
            "holdsAt(f, 5)",
            "holdsAt(f, 0)",
        )

    @Test
    fun temporal() =
        bothModes(
            Flags.defaults().temporal(true),
            "e1 :=> happens(load, 1).\ne2 :=> happens(shoot, 4).\ni1 :=> initiates(load, loaded).\n" +
                "t1 :=> terminates(shoot, loaded).\nq1 : holdsAt(loaded, 3) => canShoot.\nq2 : holdsAt(loaded, 6) => stillLoaded.",
            "canShoot",
            "stillLoaded",
            "holdsAt(loaded, 3)",
            "holdsAt(loaded, 6)",
        )

    @Test
    fun conflictsArguedOnInstances() =
        bothModes(
            Flags.defaults().graphExtensions(GraphExtension.STANDARD_PREF, GraphExtension.CONSTRAINTS).metaConflicts(true),
            "r0 : [] => conflict(p(7), q).\nf1 :=> s.\nr1 : s, {T > 1} => p(T).\nf2 :=> q.\nr2 : p(5) => ok.",
            "ok",
            "q",
            "p(7)",
        )
}
