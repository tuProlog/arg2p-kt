package it.unibo.tuprolog.argumentation.core.unit

import it.unibo.tuprolog.argumentation.core.TestingUtils.solverWithTheory
import it.unibo.tuprolog.argumentation.core.libs.graph.ArgumentLabellingMode
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.solve.SolveOptions
import it.unibo.tuprolog.unify.Unificator
import kotlin.test.Test
import kotlin.test.fail

/**
 * Query mode only builds the part of the graph a query depends on: its answers must be the ones of the whole graph,
 * projected on the query, for every semantics. Each labelling is an answer, so the distinct answers are compared
 * (a labelling of the whole graph may differ from another only outside the part the query depends on).
 */
class QueryModeTest {
    private data class Answer(
        val yes: Set<Term>,
        val no: Set<Term>,
        val und: Set<Term>,
    ) {
        override fun toString() = "in=$yes out=$no und=$und"
    }

    private val config =
        """
        graphBuildMode(standard_af).
        statementLabellingMode(statement).
        orderingPrinciple(last).
        orderingComparator(elitist).
        """.trimIndent()

    private fun solve(
        theory: String,
        goal: String,
    ): List<Map<String, Term>> {
        val solver = solverWithTheory(theory)
        return solver
            .solve(Struct.parse(goal, solver.operators), SolveOptions.allLazilyWithTimeout(Long.MAX_VALUE))
            .filter { it.isYes }
            .map { solution -> solution.substitution.entries.associate { it.key.name to it.value } }
            .toList()
    }

    private fun Term.items() = castToList().toList()

    // The answers of the whole graph: its statement labellings, projected on each query
    private fun wholeGraph(
        theory: String,
        queries: List<String>,
    ): Map<String, Set<Answer>> {
        val labellings = solve(theory, "buildLabelSets(In, Out, Und)")
        return queries.associateWith { query ->
            val goal = Term.parse(query)

            fun project(statements: Term) =
                statements
                    .items()
                    .map { it.items().single() }
                    .filter { Unificator.default.mgu(it, goal).isSuccess }
                    .toSet()
            labellings.map { Answer(project(it.getValue("In")), project(it.getValue("Out")), project(it.getValue("Und"))) }.toSet()
        }
    }

    private fun queryMode(
        theory: String,
        query: String,
    ): Set<Answer> =
        solve("$theory\nqueryMode.", "answerQuery($query, In, Out, Und)")
            .map { Answer(it.getValue("In").items().toSet(), it.getValue("Out").items().toSet(), it.getValue("Und").items().toSet()) }
            .toSet()

    private fun checkQueries(
        theory: String,
        queries: List<String>,
        semantics: Iterable<String> = ArgumentLabellingMode.values(),
        extensions: List<String> = listOf("standardPref"),
    ) {
        val graphExtensions = extensions.joinToString("\n") { "graphExtension($it)." }
        val mismatches =
            semantics.flatMap { mode ->
                val full = "$theory\n$config\n$graphExtensions\nargumentLabellingMode($mode)."
                val expected = wholeGraph(full, queries)
                queries.mapNotNull { query ->
                    val actual = queryMode(full, query)
                    if (actual == expected.getValue(query)) {
                        null
                    } else {
                        "$mode, $query: whole graph ${expected.getValue(query)}, query mode $actual"
                    }
                }
            }
        if (mismatches.isNotEmpty()) fail(mismatches.joinToString("\n"))
    }

    // A cycle of three undercuts: x -> y -> z -> x
    private val oddCycle =
        """
        rx : [] => undercut(ry).
        ry : [] => undercut(rz).
        rz : [] => undercut(rx).
        """.trimIndent()

    // a <-> b, b supports u, u -> x, and x in an odd cycle: the only semistable labelling makes b IN, as it is the
    // only one where the cycle is not UND; but the cycle is not among the arguments p depends on
    @Test
    fun semistableDependsOnAttackedArguments() =
        checkQueries(
            """
            ra : [] => p.
            rb : [] => -p.
            ru : -p => undercut(rx).
            $oddCycle
            """.trimIndent(),
            listOf("p", "-p"),
        )

    // An odd cycle, unrelated to p, leaves the whole graph without stable labellings
    @Test
    fun stableDependsOnTheWholeGraph() =
        checkQueries(
            """
            ra : [] => p.
            rb : [] => -p.
            $oddCycle
            """.trimIndent(),
            listOf("p"),
        )

    // a -> b: {b} is naive, where a is UND; but b is not among the arguments a depends on
    @Test
    fun naiveDependsOnAttackedArguments() =
        checkQueries(
            """
            ra : [] => undercut(rb).
            rb : [] => b.
            """.trimIndent(),
            listOf("undercut(rb)", "b"),
        )

    // Supports, rebuttals, a contrary, an undercut, preferences, a self-defeating chain and an even cycle
    @Test
    fun mixedTheory() =
        checkQueries(
            """
            r1 : [] => a.
            r2 : [] => b.
            r3 : a => c.
            r4 : b => -c.
            r5 : c => d.
            r6 : ~(e), a => f.
            r7 : d => e.
            r8 : f => -d.
            r9 : [] => g.
            r10 : g => -g.
            r11 : [] => h.
            r12 : -g => undercut(r11).
            r13 : [] => k.
            r14 : [] => -k.
            sup(r3, r4).
            """.trimIndent(),
            listOf("-c", "f", "h", "k"),
        )

    // Strict rules: an argument only rebutted on its defeasible sub-arguments
    @Test
    fun strictTheory() =
        checkQueries(
            """
            r1 : [] => a.
            r2 : [] => b.
            s1 : a, b -> x.
            r3 : [] => -a.
            r4 : x => y.
            r5 : [] => -y.
            sup(r5, r4).
            """.trimIndent(),
            listOf("x", "-y"),
        )

    // A directional, a connected and a whole-graph semantics, for the graph extensions
    private val someSemantics = listOf("grounded", "preferred", "semistable", "stable")

    // A preference between two rules, itself the conclusion of an argument: one that settles the rebuttal, and
    // one that is rebutted in turn
    private val defeasiblePreferences =
        listOf(
            """
            r1 : [] => a.
            r2 : [] => -a.
            r3 : b => sup(r1, r2).
            r4 : [] => b.
            """.trimIndent(),
            """
            r1 : [] => a.
            r2 : [] => -a.
            r3 : [] => sup(r1, r2).
            r4 : [] => b.
            r5 : b => -sup(r1, r2).
            r6 : [] => -b.
            """.trimIndent(),
        )

    @Test
    fun defeasiblePreferences() =
        defeasiblePreferences.forEach { checkQueries(it, listOf("a", "-a"), someSemantics, listOf("defeasiblePref")) }

    // d rests on a contested preference, which makes a lose to -a: then an odd cycle, which only a breaks, is UND.
    // The only semi-stable labelling rejects the preference, so d is OUT: the arguments the preference applies to
    // (connected to it, but not among those d depends on) decide
    @Test
    fun defeasiblePreferencesForward() =
        checkQueries(
            """
            r1 : [] => a.
            r2 : [] => -a.
            r3 : [] => sup(r2, r1).
            r4 : [] => -sup(r2, r1).
            r5 : sup(r2, r1) => d.
            r6 : a => undercut(rx).
            $oddCycle
            """.trimIndent(),
            listOf("d"),
            someSemantics,
            listOf("defeasiblePref"),
        )

    @Test
    fun defeasibleAllPreferences() =
        defeasiblePreferences.forEach { checkQueries(it, listOf("a", "-a"), someSemantics, listOf("defeasibleAllPref")) }

    // A burden of proof, itself the conclusion of an argument
    @Test
    fun burdenOfProofExtension() =
        checkQueries(
            """
            r1 : [] => a.
            r2 : [] => -a.
            r3 : [] => bp(a).
            r4 : a => c.
            """.trimIndent(),
            listOf("a", "c"),
            someSemantics,
            listOf("bp"),
        )

    // d rests on a contested burden of proof on a, which a does not meet: then an odd cycle, which only a breaks, is
    // UND. The only semi-stable labelling rejects the burden, so d is OUT: the arguments the burden is on (connected
    // to it, but not among those d depends on) decide
    @Test
    fun burdenOfProofExtensionForward() =
        checkQueries(
            """
            r1 : [] => a.
            r2 : [] => -a.
            r3 : [] => bp(a).
            r4 : [] => -bp(a).
            r5 : bp(a) => d.
            r6 : a => undercut(rx).
            $oddCycle
            """.trimIndent(),
            listOf("d"),
            listOf("semistable"),
            listOf("bp"),
        )

    @Test
    fun rebutRestriction() =
        checkQueries(
            """
            r1 : [] => a.
            r2 : a => b.
            r3 : [] => -b.
            r4 : -b => -a.
            """.trimIndent(),
            listOf("b", "-b"),
            someSemantics,
            listOf("rebutRestriction", "standardPref"),
        )

    // Burdens of proof declared in the theory, for the bp semantics
    @Test
    fun burdenOfProofSemantics() =
        checkQueries(
            """
            r1 : [] => a.
            r2 : [] => -a.
            r3 : a => c.
            r4 : [] => -c.
            bp(-a).
            """.trimIndent(),
            listOf("a", "c"),
            listOf("bp_grounded", "bp_grounded_partial", "bp_grounded_complete"),
        )

    // With metaConflicts the conflicts are the conclusions of arguments
    @Test
    fun metaConflicts() =
        checkQueries(
            """
            metaConflicts.
            r1 : [] => a.
            r2 : [] => b.
            r3 : [] => conflict(a, b).
            """.trimIndent(),
            listOf("a", "b"),
            someSemantics + "naive",
        )

    // a <-> c, and a conflicts with b, in an odd cycle only a breaks: the only semi-stable labelling has a IN, as
    // the arguments a conflicts with (connected to it, but not among those it depends on) decide
    @Test
    fun metaConflictsForward() =
        checkQueries(
            """
            metaConflicts.
            r1 : [] => a.
            r2 : [] => c.
            r3 : [] => conflict(a, c).
            r4 : [] => conflict(c, a).
            r5 : [] => conflict(a, b).
            r6 : [] => b.
            r7 : [] => x.
            r8 : [] => y.
            r9 : [] => conflict(b, x).
            r10 : [] => conflict(x, y).
            r11 : [] => conflict(y, b).
            """.trimIndent(),
            listOf("a"),
            listOf("semistable"),
        )

    // With metaRules a rule only applies where applicable(Rule) holds
    @Test
    fun metaRules() =
        checkQueries(
            """
            metaRules.
            f1 :=> applicable(r1).
            r1 : [] => a.
            r2 : a => b.
            """.trimIndent(),
            listOf("a", "b"),
            someSemantics,
        )
}
