package it.unibo.tuprolog.argumentation.core.libraries

import it.unibo.tuprolog.argumentation.Flags
import it.unibo.tuprolog.argumentation.core.Arg2pSolverFactory
import it.unibo.tuprolog.argumentation.engine.vm.program.SharedPrograms
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.solve.MutableSolver
import it.unibo.tuprolog.solve.library.Runtime
import it.unibo.tuprolog.theory.Theory
import it.unibo.tuprolog.theory.parsing.parse
import java.util.concurrent.Callable
import java.util.concurrent.Executors
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertTrue

/** The solvers of a process share the code of the modules, and a solver reuses the conversion of its theory */
class SharedProgramsTest {
    private val theory = "f0 :=> a0.\n" + (1 until 20).joinToString("") { "r$it : a${it - 1} => a$it.\n" } + "p : a3 => b.\nq : a3 => -b.\n"

    private fun solver(text: String): MutableSolver = Arg2pSolverFactory.default(theory = text, settings = Flags.defaults().library())

    private fun labels(solver: MutableSolver): String {
        val goal = Struct.parse("solve(I, O, U)", solver.operators)
        return solver
            .solve(goal)
            .map { it.toString().replace(Regex("""_\d+"""), "") }
            .toList()
            .toString()
    }

    private fun labels(mode: String): String = labels(solver("$theory argumentLabellingMode($mode).\n"))

    private class Instanced : PrologLibrary("instanced") {
        override val prologRawTheory: String = "one(1).\ntwo(X) :- one(X).\n"
    }

    @Test
    fun solversOnThreadsGiveTheLabelsTheyGiveAlone() {
        val modes = listOf("grounded", "preferred", "complete", "stable")
        val alone = modes.associateWith { labels(it) }
        val pool = Executors.newFixedThreadPool(8)
        try {
            val runs = (0 until 400).map { i -> modes[i % modes.size].let { mode -> mode to pool.submit(Callable { labels(mode) }) } }
            runs.forEach { (mode, run) -> assertEquals(alone[mode], run.get(), mode) }
        } finally {
            pool.shutdownNow()
        }
    }

    // What made the base program grow without end: a set of flags, a theory or an instance of a module of its own
    @Test
    fun theSharedCodeGrowsWithTheModulesAlone() {
        val modes = listOf("grounded", "preferred", "complete", "stable")
        modes.forEach { labels(it) }
        val shared = SharedPrograms.codeSize
        assertTrue(shared > 1000, "the modules are shared: $shared")
        repeat(30) { i -> labels(solver("$theory extra$i(a).\nargumentLabellingMode(${modes[i % modes.size]}).\n")) }
        assertEquals(shared, SharedPrograms.codeSize)
        // the instances of a module share its code
        val program = SharedPrograms.derived()
        program.compile(Runtime.of(Instanced().baseContent))
        val after = SharedPrograms.codeSize
        repeat(5) { SharedPrograms.derived().compile(Runtime.of(Instanced().baseContent)) }
        assertEquals(after, SharedPrograms.codeSize)
    }

    @Test
    fun theConversionFollowsTheTheory() {
        val rules = "r1 : [] => a.\nr2 : [] => -a.\n"
        val preferred = labels(solver(rules + "sup(r1, r2).\n"))
        val solver = solver(rules)
        val without = labels(solver)
        assertEquals(without, labels(solver), "a second solve")
        assertTrue(without != preferred, without)
        // a clause asserted in the theory, by a goal or through the solver
        solver.solve(Struct.parse("assertz(sup(r1, r2))", solver.operators)).first()
        assertEquals(preferred, labels(solver), "asserted by a goal")
        val loaded = solver(rules)
        labels(loaded)
        loaded.loadStaticKb(Theory.parse(rules + "sup(r1, r2).\n", loaded.operators))
        assertEquals(preferred, labels(loaded), "another theory")
    }

    @Test
    fun readingTheTheoryKeepsWhatABranchHolds() {
        val solver = solver("r1 : [] => a.\n")
        labels(solver)
        val kept =
            solver
                .solve(
                    Struct.parse(
                        "contextReset, contextAssert(mine(1)), store::rulesRead, contextCheck(mine(X)), store::rule(_, _, _)",
                        solver.operators,
                    ),
                ).first()
        assertTrue(kept.isYes, kept.toString())
    }
}
