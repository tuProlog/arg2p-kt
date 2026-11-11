package it.unibo.tuprolog.argumentation.core

import it.unibo.tuprolog.argumentation.Flags
import it.unibo.tuprolog.argumentation.core.dsl.arg2pScope
import it.unibo.tuprolog.argumentation.core.model.Graph
import it.unibo.tuprolog.argumentation.core.model.LabelledArgument
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.dsl.logicProgramming
import it.unibo.tuprolog.solve.MutableSolver
import it.unibo.tuprolog.solve.Solution
import it.unibo.tuprolog.solve.SolveOptions
import it.unibo.tuprolog.solve.Solver
import it.unibo.tuprolog.solve.TimeDuration
import it.unibo.tuprolog.solve.assertSolutionEquals
import it.unibo.tuprolog.solve.yes
import it.unibo.tuprolog.theory.parsing.parse
import it.unibo.tuprolog.solve.Solver as BaseSolver

object TestingUtils {
    private val duration: TimeDuration
        get() = Long.MAX_VALUE

    fun solver() = solverWithTheory("")

    fun solverWithTheory(theory: String) = Arg2pSolverFactory.default(theory = theory, settings = Flags.none().library())

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

    fun theoryLabels(
        theory: String,
        argsIn: String,
        argsOut: String,
        argsUnd: String,
    ): Solver =
        logicProgramming {
            solverWithTheory(theory).also { solver ->
                testGoalNoBacktracking(
                    "solve"("StatIn", "StatOut", "StatUnd"),
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

    fun prepareContext(
        solver: MutableSolver,
        graph: Graph,
    ) = arg2pScope {
        graph.arguments.forEach {
            solver.solve("store" call "saveArgument"("test", logicList(Term.parse(it.conclusion)), it.termRepresentation())).first()
        }
        graph.attacks.forEach {
            solver
                .solve(
                    "store" call
                        "saveAttack"(
                            it.type ?: "none",
                            it.attacker.termRepresentation(),
                            it.target.termRepresentation(),
                            it.on?.termRepresentation() ?: "none",
                        ),
                ).first()
        }
        graph.supports.forEach {
            solver.solve("store" call "saveSupport"(it.supporter.termRepresentation(), it.supported.termRepresentation())).first()
        }
    }

    fun checkResults(
        solver: MutableSolver,
        labellings: Iterable<LabelledArgument>,
    ) = arg2pScope {
        labellings.forEach {
            testYesGoal("contextCheck"(it.toTerm()), solver)
        }
    }

    fun checkResults(
        solver: MutableSolver,
        graph: Graph,
    ) = arg2pScope {
        graph.arguments.forEach {
            testYesGoal("contextCheck"(it.toTerm()), solver)
        }
        // the attacks as the graph store reads them, extended to the arguments built on the attacked ones
        graph.attacks.forEach {
            testYesGoal("store" call it.toTerm(), solver)
        }
        graph.supports.forEach {
            testYesGoal("contextCheck"(it.toTerm()), solver)
        }
    }
}
