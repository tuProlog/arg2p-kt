package it.unibo.argumentation.arg2p

import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.solve.*
import it.unibo.tuprolog.solve.library.Libraries
import it.unibo.tuprolog.theory.Theory
import it.unibo.tuprolog.theory.parsing.parse


object TestingUtils {

    private val duration: TimeDuration
        get() = Long.MAX_VALUE

    fun withArgOperators(theory: String) =
        Theory.parse(("""
                    :- op(1199, xfx, '=>').
                    :- op(1001, xfx, ':').
                """ + theory).trimIndent())


    fun solver(theory : Theory = Theory.empty()) =
        ClassicSolverFactory.solverWithDefaultBuiltins(
            otherLibraries = Libraries.of(Arg2p),
            staticKb = theory
        )

    fun testGoal(goal: Struct, solver : Solver = solver(), expectedSolutions: (Struct) -> Iterable<Solution>) {
        val solutions = solver.solve(goal, duration).toList()
        assertSolutionEquals(
            expectedSolutions(goal),
            solutions
        )
    }

    fun testGoalNoBacktracking(goal: Struct, solver : Solver = solver(), expectedSolutions: (Struct) -> Solution) {
        val solution = solver.solve(goal, duration).first()
        assertSolutionEquals(
            listOf(expectedSolutions(goal)),
            listOf(solution)
        )
    }

    fun testYesGoal(goal: Struct, solver : Solver = solver()) {
        testGoal(goal, solver) {
            listOf(it.yes())
        }
    }

    fun testNoGoal(goal: Struct, solver : Solver = solver()) {
        testGoal(goal, solver) {
            listOf(it.no())
        }
    }

    fun testYesGoal(goals: Iterable<Struct>, solver : Solver = solver()) =
        goals.forEach { testYesGoal(it, solver) }


    fun testNoGoal(goals: Iterable<Struct>, solver : Solver = solver()) =
        goals.forEach { testNoGoal(it, solver) }

}