package it.unibo.tuprolog.argumentation.core.unit

import it.unibo.tuprolog.argumentation.core.Arg2pSolverFactory
import it.unibo.tuprolog.argumentation.core.TestingUtils.prepareContext
import it.unibo.tuprolog.argumentation.core.TestingUtils.solver
import it.unibo.tuprolog.argumentation.core.dsl.arg2pScope
import it.unibo.tuprolog.argumentation.core.libs.basic.FlagsBuilder
import it.unibo.tuprolog.argumentation.core.mining.graph
import it.unibo.tuprolog.argumentation.core.model.Argument
import it.unibo.tuprolog.argumentation.core.model.Attack
import it.unibo.tuprolog.argumentation.core.model.Graph
import it.unibo.tuprolog.argumentation.core.model.LabelledArgument
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import kotlin.test.Test
import kotlin.test.assertTrue

/**
 * Examples from P. Baroni, M. Caminada, M. Giacomin,
 * "An introduction to argumentation semantics", The Knowledge Engineering Review 26(4), 2011.
 *
 * Frameworks are named after the paper figures. Attacks are written as "XY" (X attacks Y),
 * expected labellings as (in, out) pairs, every other argument being undecided.
 * Conflict-free and admissible results are the Ext2Lab images of the conflict-free/admissible sets.
 * Results not stated verbatim in the paper, but derived from its definitions, are marked "derived".
 */
class SemanticsPaperExamplesTest {
    private class Framework(
        arguments: String,
        vararg attacks: String,
    ) {
        val arguments = arguments.associateWith { Argument(listOf("r$it"), "r$it", it.lowercase()) }
        val attacks = attacks.map { Attack(this.arguments.getValue(it[0]), this.arguments.getValue(it[1])) }
    }

    private fun check(
        framework: Framework,
        expectations: Map<String, List<Pair<String, String>>>,
    ) {
        val failures =
            expectations.mapNotNull { (semantics, labellings) ->
                val expected =
                    labellings.map { (inArgs, outArgs) ->
                        framework.arguments
                            .map { (name, argument) ->
                                when (name) {
                                    in inArgs -> "in"
                                    in outArgs -> "out"
                                    else -> "und"
                                } + argument.topRule
                            }.toSet()
                    }
                val actual = solve(framework, semantics)
                if (expected.toSet() == actual.toSet() && expected.size == actual.size) {
                    null
                } else {
                    "$semantics: expected $expected but was $actual"
                }
            }
        assertTrue(failures.isEmpty(), failures.joinToString("\n"))
    }

    private fun solve(
        framework: Framework,
        semantics: String,
    ): List<Set<String>> =
        arg2pScope {
            solver().let { solver ->
                prepareContext(
                    solver,
                    Graph.of(
                        framework.arguments.values.map { LabelledArgument(it, "und") },
                        framework.attacks,
                        emptyList(),
                    ),
                )
                solver
                    .solve(semantics call "argumentLabelling")
                    .filter { it.isYes }
                    .map { _ ->
                        solver
                            .graph()
                            .labellings
                            .map { it.label + it.argument.topRule }
                            .toSet()
                    }.toList()
            }
        }

    // Figure 4: A simple argumentation framework
    @Test
    fun figure4() =
        check(
            Framework("ABCD", "AB", "BC", "CD", "DC"),
            mapOf(
                "conflictfree" to
                    listOf(
                        "" to "",
                        "A" to "B",
                        "B" to "C",
                        "C" to "D",
                        "D" to "C",
                        "AC" to "BD",
                        "AD" to "BC",
                        "BD" to "C",
                    ),
                "admissible" to listOf("" to "", "A" to "B", "D" to "C", "AC" to "BD", "AD" to "BC"),
                "complete" to listOf("A" to "B", "AC" to "BD", "AD" to "BC"),
                "grounded" to listOf("A" to "B"),
                "preferred" to listOf("AC" to "BD", "AD" to "BC"),
                "stable" to listOf("AC" to "BD", "AD" to "BC"),
                "semistable" to listOf("AC" to "BD", "AD" to "BC"),
                "ideal" to listOf("A" to "B"),
                // derived: semi-stable and preferred extensions coincide, so eager = ideal
                "eager" to listOf("A" to "B"),
                "stage" to listOf("AC" to "BD", "AD" to "BC"),
                "cf2" to listOf("AC" to "BD", "AD" to "BC"),
            ),
        )

    // Figure 5: The case of "floating" acceptance
    @Test
    fun figure5() =
        check(
            Framework("ABCD", "AB", "BA", "AC", "BC", "CD"),
            mapOf(
                "admissible" to listOf("" to "", "A" to "BC", "B" to "AC", "AD" to "BC", "BD" to "AC"),
                "complete" to listOf("" to "", "AD" to "BC", "BD" to "AC"),
                "grounded" to listOf("" to ""),
                "preferred" to listOf("AD" to "BC", "BD" to "AC"),
                "stable" to listOf("AD" to "BC", "BD" to "AC"),
                "semistable" to listOf("AD" to "BC", "BD" to "AC"),
                "ideal" to listOf("" to ""),
                // derived: semi-stable and preferred extensions coincide, so eager = ideal
                "eager" to listOf("" to ""),
                "stage" to listOf("AD" to "BC", "BD" to "AC"),
                "cf2" to listOf("AD" to "BC", "BD" to "AC"),
            ),
        )

    // Figure 6: Cycle of three attacking arguments
    @Test
    fun figure6() =
        check(
            Framework("ABC", "AB", "BC", "CA"),
            mapOf(
                "conflictfree" to listOf("" to "", "A" to "B", "B" to "C", "C" to "A"),
                "admissible" to listOf("" to ""),
                "complete" to listOf("" to ""),
                "grounded" to listOf("" to ""),
                "preferred" to listOf("" to ""),
                "stable" to emptyList(),
                "semistable" to listOf("" to ""),
                "ideal" to listOf("" to ""),
                // derived: the only semi-stable labelling is the trivial one
                "eager" to listOf("" to ""),
                "stage" to listOf("A" to "B", "B" to "C", "C" to "A"),
                "cf2" to listOf("A" to "B", "B" to "C", "C" to "A"),
            ),
        )

    // Figure 8: An argumentation framework where preferred and stable semantics differ
    @Test
    fun figure8() =
        check(
            Framework("ABCDE", "AB", "BA", "BC", "CD", "DE", "EC"),
            mapOf(
                "complete" to listOf("" to "", "A" to "B", "BD" to "ACE"),
                "grounded" to listOf("" to ""),
                "preferred" to listOf("A" to "B", "BD" to "ACE"),
                "stable" to listOf("BD" to "ACE"),
                "semistable" to listOf("BD" to "ACE"),
                // derived: the preferred extensions {A} and {B, D} have an empty intersection
                "ideal" to listOf("" to ""),
                // derived: the only semi-stable extension {B, D} is complete
                "eager" to listOf("BD" to "ACE"),
                // stable extensions exist, hence stage = stable
                "stage" to listOf("BD" to "ACE"),
                "cf2" to listOf("AC" to "BD", "AD" to "BE", "AE" to "BC", "BD" to "ACE"),
            ),
        )

    // Figure 9: The ideal labelling can be less skeptical than the grounded labelling
    @Test
    fun figure9() =
        check(
            Framework("AB", "AB", "BA", "BB"),
            mapOf(
                "grounded" to listOf("" to ""),
                "ideal" to listOf("A" to "B"),
                // derived: {A} is the only non-empty admissible set
                "preferred" to listOf("A" to "B"),
                "eager" to listOf("A" to "B"),
            ),
        )

    // Figure 10: Stage semantics differs from semi-stable semantics
    @Test
    fun figure10() =
        check(
            Framework("AB", "AA", "AB"),
            mapOf(
                "complete" to listOf("" to ""),
                "grounded" to listOf("" to ""),
                "preferred" to listOf("" to ""),
                "stable" to emptyList(),
                "semistable" to listOf("" to ""),
                "ideal" to listOf("" to ""),
                "eager" to listOf("" to ""),
                "stage" to listOf("B" to ""),
            ),
        )

    // Figure 11: A peculiar case for stage semantics
    @Test
    fun figure11() =
        check(
            Framework("ABC", "AB", "BC", "CC"),
            mapOf(
                // derived: A is unattacked, so it is in; C attacks itself, so it stays undecided
                "grounded" to listOf("A" to "B"),
                "semistable" to listOf("A" to "B"),
                "stage" to listOf("A" to "B", "B" to "C"),
                "cf2" to listOf("A" to "B"),
            ),
        )

    // Figure 13: Cycle of three attacking arguments in turn attacking another argument
    @Test
    fun figure13() =
        check(
            Framework("ABCD", "AB", "BC", "CA", "AD", "BD", "CD"),
            mapOf(
                "complete" to listOf("" to ""),
                "grounded" to listOf("" to ""),
                "preferred" to listOf("" to ""),
                "stable" to emptyList(),
                "semistable" to listOf("" to ""),
                "ideal" to listOf("" to ""),
                "eager" to listOf("" to ""),
                "stage" to listOf("A" to "BD", "B" to "CD", "C" to "AD"),
                "cf2" to listOf("A" to "BD", "B" to "CD", "C" to "AD"),
            ),
        )

    // Figure 5 through abstract::solve: every argument is labelled, none is repeated within a label
    @Test
    fun abstractSolveHasNoRepeatedArguments() {
        val query = "abstract::solve([a, b, c, d], [(a, b), (b, a), (a, c), (b, c), (c, d)], In, Out, Und)"
        val failures =
            listOf("complete", "preferred", "semistable", "stable", "ideal", "eager", "naive", "stage", "cf2", "stage2")
                .mapNotNull { semantics ->
                    val solver = Arg2pSolverFactory.default(settings = FlagsBuilder().argumentLabellingMode(semantics).create())
                    val solutions = solver.solve(Struct.parse(query, solver.operators)).toList()
                    // every labelling labels the four arguments
                    val labellings =
                        solutions
                            .filter { it.isYes }
                            .map { solution -> solution.substitution.values.sumOf { it.castToList().toList().size } }
                    if (labellings.isEmpty() || labellings.any { it != 4 }) {
                        return@mapNotNull "$semantics: labellings of $labellings arguments"
                    }
                    val duplicates =
                        solutions.filter { it.isYes }.flatMap { it.substitution.values }.any { labels ->
                            (labels as it.unibo.tuprolog.core.List).toList().let { it.size != it.toSet().size }
                        }
                    if (duplicates) "$semantics: repeated arguments in $solutions" else null
                }
        assertTrue(failures.isEmpty(), failures.joinToString("\n"))
    }
}
