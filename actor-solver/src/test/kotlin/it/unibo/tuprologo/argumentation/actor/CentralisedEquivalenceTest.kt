package it.unibo.tuprologo.argumentation.actor

import it.unibo.tuprolog.argumentation.actor.actors.ResponseConsumer
import it.unibo.tuprolog.argumentation.actor.message.Add
import it.unibo.tuprolog.argumentation.core.Arg2pSolver
import it.unibo.tuprolog.argumentation.core.dsl.arg2pScope
import it.unibo.tuprolog.argumentation.core.engine.Arg2pPrologSolverFactory
import it.unibo.tuprolog.argumentation.core.runtime.settings.FlagsBuilder
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.solve.flags.FlagStore
import it.unibo.tuprolog.solve.flags.TrackVariables
import it.unibo.tuprolog.solve.flags.Unknown
import it.unibo.tuprolog.theory.Theory
import it.unibo.tuprolog.theory.parsing.parse
import org.junit.Test
import kotlin.test.assertEquals

// The labels of the arguments for a statement are the same in the actor solver, its rules split among workers, and in
// the standard solver, on the whole graph of the same rules (no preferences)
class CentralisedEquivalenceTest {
    // The number of arguments for the statement labelled in, out and und
    private data class Labels(
        val inArgs: Int,
        val outArgs: Int,
        val undArgs: Int,
    )

    private fun actor(
        rules: List<String>,
        statement: String,
    ): Labels {
        var labels = Labels(0, 0, 0)
        withActorSystem { system, master ->
            rules.forEach { master.tell(Add(it)) }
            ResponseConsumer.getResponse(statement, system, master).also {
                labels = Labels(it.inArgs.size, it.outArgs.size, it.undArgs.size)
            }
        }
        return labels
    }

    private fun centralised(
        rules: List<String>,
        statement: String,
    ): Labels =
        arg2pScope {
            Arg2pPrologSolverFactory
                .mutableSolverWithDefaultBuiltins(
                    otherLibraries =
                        Arg2pSolver
                            .default(listOf(FlagsBuilder(graphExtensions = emptyList()).create()))
                            .to2pLibraries(),
                    flags = FlagStore.DEFAULT.set(Unknown, Unknown.FAIL).set(TrackVariables, TrackVariables.ON),
                ).let { solver ->
                    solver.loadStaticKb(Theory.parse(rules.joinToString("\n") { "$it." }, solver.operators))
                    val claim = Struct.parse(statement, solver.operators)
                    val count = { label: String ->
                        val solution =
                            solver
                                .solve(
                                    "buildLabelSetsSilent" and
                                        "findall"(
                                            "x",
                                            ("graphStore" call "argumentLabel"(label, A)) and ("argument" call "claim"(A, claim)),
                                            L,
                                        ) and "length"(L, N),
                                ).first()
                        solution.substitution[N].toString().toInt()
                    }
                    Labels(count("in"), count("out"), count("und"))
                }
        }

    private fun assertSameLabels(
        rules: List<String>,
        vararg statements: String,
    ) = statements.forEach { assertEquals(centralised(rules, it), actor(rules, it), "the arguments for $it") }

    @Test
    fun premisesAreNotUndercut() =
        assertSameLabels(
            listOf("r1 :=> b", "r3 : c => d", "r2 : b => c", "r4 : d => e", "r5 :=> undercut(r1)"),
            "b",
            "c",
            "undercut(r1)",
        )

    @Test
    fun undercutsOfTheRulesOfSubArguments() =
        assertSameLabels(
            listOf("r1 :=> b", "r2 : b => c", "r3 : c => d", "r5 :=> undercut(r2)"),
            "c",
            "d",
        )

    @Test
    fun attacksOnSubArgumentsFromAnotherWorker() =
        assertSameLabels(
            listOf("r1 :=> b", "r2 : b => c", "r3 : c => d", "r4 :=> -b"),
            "b",
            "c",
            "d",
            "-b",
        )

    @Test
    fun rebutsOfSubArgumentsFromAnotherWorker() =
        assertSameLabels(
            listOf("r1 :=> a", "r2 : a => b", "r3 : b => c", "r4 :=> e", "r5 : e => -b"),
            "b",
            "c",
            "-b",
        )

    @Test
    fun reinstatement() =
        assertSameLabels(
            listOf("r1 :=> a", "r2 : a => b", "r3 :=> c", "r4 : c => -b", "r5 :=> undercut(r4)"),
            "b",
            "-b",
        )

    @Test
    fun strictRulesAreNotUndercut() =
        assertSameLabels(
            listOf("r1 :-> a", "r2 : a -> b", "r3 :=> undercut(r2)", "r4 :=> -b"),
            "b",
            "-b",
        )

    @Test
    fun severalAttackersFromTheSameWorker() =
        assertSameLabels(
            listOf("r1 :=> a", "r2 : a => b", "r3 :=> c", "r4 : c => -b", "r5 : c => undercut(r2)", "r6 :=> -c"),
            "b",
            "-b",
            "c",
        )

    @Test
    fun severalArgumentsFromTheSameWorker() =
        assertSameLabels(
            listOf("r1 :=> a", "r2 : a => b", "r3 : a => b", "r4 :=> c", "r5 : c => b", "r6 :=> -a"),
            "b",
            "a",
        )
}
