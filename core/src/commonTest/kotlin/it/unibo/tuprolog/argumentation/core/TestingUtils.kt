package it.unibo.tuprolog.argumentation.core

import it.unibo.tuprolog.argumentation.core.dsl.arg2pScope
import it.unibo.tuprolog.argumentation.core.model.Graph
import it.unibo.tuprolog.argumentation.core.model.LabelledArgument
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.dsl.logicProgramming
import it.unibo.tuprolog.solve.MutableSolver
import it.unibo.tuprolog.solve.Solution
import it.unibo.tuprolog.solve.SolveOptions
import it.unibo.tuprolog.solve.Solver
import it.unibo.tuprolog.solve.TimeDuration
import it.unibo.tuprolog.solve.assertSolutionEquals
import it.unibo.tuprolog.solve.flags.FlagStore
import it.unibo.tuprolog.solve.flags.TrackVariables
import it.unibo.tuprolog.solve.flags.Unknown
import it.unibo.tuprolog.solve.no
import it.unibo.tuprolog.solve.yes
import it.unibo.tuprolog.theory.Theory
import it.unibo.tuprolog.theory.parsing.parse
import it.unibo.tuprolog.solve.Solver as BaseSolver

object TestingUtils {
    private val duration: TimeDuration
        get() = Long.MAX_VALUE

    fun withArgOperators(theory: String) = Theory.parse(theory, Arg2pSolver.default().operators())

    fun solver(
        theory: Theory = Theory.empty(),
        flags: FlagStore = FlagStore.DEFAULT,
    ) = Solver.prolog.mutableSolverWithDefaultBuiltins(
        otherLibraries = Arg2pSolver.default().to2pLibraries(),
        staticKb = theory,
        flags = flags.set(Unknown, Unknown.FAIL).set(TrackVariables, TrackVariables.ON),
    )

    fun solverWithTheory(theory: String) = solver(withArgOperators(theory))

    fun testGoal(
        goal: Struct,
        solver: BaseSolver = solver(),
        expectedSolutions: (Struct) -> Iterable<Solution>,
    ) {
        val solutions = solver.solve(goal, SolveOptions.allLazilyWithTimeout(duration)).toList()
        assertSolutionEquals(
            expectedSolutions(goal),
            solutions,
        )
    }

    fun testGoalNoBacktracking(
        goal: Struct,
        solver: BaseSolver = solver(),
        expectedSolutions: (Struct) -> Solution,
    ) {
        val solution = solver.solve(goal, SolveOptions.allLazilyWithTimeout(duration)).first()
        assertSolutionEquals(
            listOf(expectedSolutions(goal)),
            listOf(solution),
        )
    }

    fun testYesGoal(
        goal: Struct,
        solver: BaseSolver = solver(),
    ) {
        testGoal(goal, solver) {
            listOf(it.yes())
        }
    }

    fun testNoGoal(
        goal: Struct,
        solver: BaseSolver = solver(),
    ) {
        testGoal(goal, solver) {
            listOf(it.no())
        }
    }

    fun testYesGoal(
        goals: Iterable<Struct>,
        solver: BaseSolver = solver(),
    ) = goals.forEach { testYesGoal(it, solver) }

    fun testNoGoal(
        goals: Iterable<Struct>,
        solver: BaseSolver = solver(),
    ) = goals.forEach { testNoGoal(it, solver) }

    fun buildLabelSets(
        theory: String,
        argsIn: String,
        argsOut: String,
        argsUnd: String,
    ): Solver {
        return logicProgramming {
            solverWithTheory(theory).also { solver ->
                testGoalNoBacktracking(
                    "buildLabelSets"("StatIn", "StatOut", "StatUnd"),
                    solver,
                ) {
                    it.yes(
                        "StatIn" to Struct.parse(argsIn),
                        "StatOut" to Struct.parse(argsOut),
                        "StatUnd" to Struct.parse(argsUnd),
                    )
                }
            }
        }
    }

    fun answerQuery(
        theory: String,
        query: String,
        argsIn: String,
        argsOut: String,
        argsUnd: String,
    ) {
        logicProgramming {
            solverWithTheory(theory).also { solver ->
                TestingUtils.testGoalNoBacktracking(
                    "answerQuery"(Struct.parse(query), "StatIn", "StatOut", "StatUnd"),
                    solver,
                ) {
                    it.yes(
                        "StatIn" to Struct.parse(argsIn),
                        "StatOut" to Struct.parse(argsOut),
                        "StatUnd" to Struct.parse(argsUnd),
                    )
                }
            }
        }
    }

    fun prepareContext(
        solver: MutableSolver,
        theory: it.unibo.tuprolog.argumentation.core.model.Theory,
    ) = arg2pScope {
        theory.forEach {
            solver.solve("context_assert"(it.toTerm())).first()
        }
    }

    fun prepareContext(
        solver: MutableSolver,
        graph: Graph,
    ) = arg2pScope {
        graph.arguments.forEach {
            solver.solve("context_assert"(it.toTerm())).first()
        }
        graph.attacks.forEach {
            solver.solve("context_assert"(it.toTerm())).first()
        }
        graph.supports.forEach {
            solver.solve("context_assert"(it.toTerm())).first()
        }
    }

    fun checkResults(
        solver: MutableSolver,
        labellings: Iterable<LabelledArgument>,
    ) = arg2pScope {
        labellings.forEach {
            testYesGoal("context_check"(it.toTerm()), solver)
        }
    }

    fun checkResults(
        solver: MutableSolver,
        graph: Graph,
    ) = arg2pScope {
        graph.arguments.forEach {
            testYesGoal("context_check"(it.toTerm()), solver)
        }
        graph.attacks.forEach {
            testYesGoal("context_check"(it.toTerm()), solver)
        }
        graph.supports.forEach {
            testYesGoal("context_check"(it.toTerm()), solver)
        }
    }
}
