package it.unibo.tuprolog.argumentation.core.unit

import it.unibo.tuprolog.argumentation.core.TestingUtils.solverWithTheory
import it.unibo.tuprolog.argumentation.core.flags.ArgumentLabellingMode
import it.unibo.tuprolog.core.Atom
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Substitution
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
    // Statements compared up to the names of their variables
    private data class Answer(
        val yes: Set<String>,
        val no: Set<String>,
        val und: Set<String>,
    ) {
        override fun toString() = "in=$yes out=$no und=$und"
    }

    private fun Iterable<Term>.canonical() =
        map { term ->
            val variables = term.variables.distinct().toList()
            term.apply(Substitution.of(variables.mapIndexed { i, v -> v to Atom.of("_V$i") }.toMap())).toString()
        }.toSet()

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
                    .canonical()
            labellings.map { Answer(project(it.getValue("In")), project(it.getValue("Out")), project(it.getValue("Und"))) }.toSet()
        }
    }

    private fun queryMode(
        theory: String,
        query: String,
    ): Set<Answer> =
        solve("$theory\nqueryMode.", "answerQuery($query, In, Out, Und)")
            .map { answer -> Answer(answer.labelled("In"), answer.labelled("Out"), answer.labelled("Und")) }
            .toSet()

    private fun Map<String, Term>.labelled(label: String) = getValue(label).items().canonical()

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

    // Rules in a cycle: a statement is met again while its arguments are being built, and no argument uses a rule
    // twice
    @Test
    fun cyclicRules() =
        checkQueries(
            """
            f :=> a.
            r1 : a => b.
            r2 : b => c.
            r3 : c => a.
            r4 : [] => -c.
            """.trimIndent(),
            listOf("a", "b", "c", "-c"),
        )

    // Strict rules and their transpositions form cycles
    @Test
    fun transposedStrictRules() =
        checkQueries(
            """
            f1 :=> a.
            f2 :=> b.
            s1 : a, b -> c.
            s2 : c -> d.
            r1 : [] => -d.
            autoTransposition.
            """.trimIndent(),
            listOf("c", "d", "-d", "-a"),
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

    // Rule names with variables, so that undercutters can target the instances of a rule
    @Test
    fun parametricUndercuts() =
        checkQueries(
            """
            f1 :=> a(1).
            f2 :=> a(2).
            f3 :=> c(2).
            r(X) : a(X) => b(X).
            u(X) : c(X) => undercut(r(X)).
            s(X) : b(X) => d(X).
            """.trimIndent(),
            listOf("b(X)", "b(1)", "b(2)", "d(X)", "undercut(r(X))"),
            someSemantics + "naive",
        )

    // Prolog checks among the premises, binding and testing the variables of the rules
    @Test
    fun prologPremises() =
        checkQueries(
            """
            f1 :=> a(1).
            f2 :=> a(2).
            f3 :=> a(3).
            r(X) : a(X), prolog(X > 1) => big(X).
            s(X) : a(X), prolog(X < 3) => -big(X).
            t(X) : big(X), prolog(Y is X + 1) => next(Y).
            """.trimIndent(),
            listOf("big(X)", "-big(X)", "next(X)", "big(2)"),
            someSemantics + "naive",
        )

    // Weak negation: a contrary premise, attacked by any argument for what it assumes false
    @Test
    fun weakNegation() =
        checkQueries(
            """
            f1 :=> a(1).
            f2 :=> a(2).
            f3 :=> c(2).
            r(X) : a(X), ~(abnormal(X)) => ok(X).
            s(X) : c(X) => abnormal(X).
            t(X) : ok(X) => -abnormal(X).
            """.trimIndent(),
            listOf("ok(X)", "ok(1)", "abnormal(X)", "-abnormal(X)"),
            someSemantics + "naive",
        )

    // Premises supported most selective first: b(k) only needs the a(X) that c(X, k) selects. Prolog checks run
    // once the premises before them are supported: one waiting for the variable it tests, one binding the variable
    // of the premise after it
    @Test
    fun selectivePremises() =
        checkQueries(
            (1..20).joinToString("\n") { "f$it :=> a($it)." } +
                """

                g1 :=> c(5, k).
                g2 :=> c(7, h).
                g3 :=> -a(7).
                g4 :=> q(10).
                r(X, Y) : a(X), c(X, Y) => b(Y).
                s(X) : a(X), prolog(X > 18) => big(X).
                t(X) : a(X), prolog(Y is X * 2), q(Y) => double(X).
                """.trimIndent(),
            listOf("b(k)", "b(Y)", "big(X)", "big(19)", "double(X)", "double(5)"),
            listOf("grounded", "preferred"),
        )

    // Variables, Prolog checks, weak negation and parametric undercuts together, in chains
    @Test
    fun variablesPrologAndWeakNegation() =
        checkQueries(
            """
            f1 :=> n(1).
            f2 :=> n(2).
            f3 :=> n(3).
            f4 :=> flag(2).
            r(X) : n(X), prolog(X < 3), ~(excluded(X)) => p(X).
            s(X) : p(X), prolog(Y is X * 2) => q(X, Y).
            e(X) : flag(X) => excluded(X).
            u(X, Y) : q(X, Y), prolog(Y > 2) => undercut(r(X)).
            v(X) : n(X), ~(p(X)) => -q(X, _).
            w(X) : q(X, _) => undercut(e(X)).
            """.trimIndent(),
            listOf("p(X)", "q(X, Y)", "-q(X, Y)", "excluded(X)", "p(1)"),
            someSemantics + "naive",
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
