package it.unibo.tuprolog.argumentation.engine.vm

import it.unibo.tuprolog.argumentation.engine.solverWithTheory
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.solve.library.Library
import it.unibo.tuprolog.solve.library.Runtime
import it.unibo.tuprolog.theory.Theory
import it.unibo.tuprolog.theory.parsing.parse
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.time.Duration.Companion.seconds

/**
 * The clauses the theory of the user gives a builtin are its alternatives: also when the builtin is reached by an inline
 * instruction that left it a case (is/2 of a float), in a clause compiled with no such theory
 * (a library's). Backtracking into them goes on after the instruction, in the clause that ran it, with that clause's
 * continuation and registers.
 */
class BuiltinTheoryAlternativesTest {
    private fun solve(
        theory: String,
        goal: String,
        library: String = "",
    ): String {
        val libraries =
            if (library.isEmpty()) {
                Runtime.empty()
            } else {
                Runtime.of(Library.of("test.behind", clauses = Theory.parse(library).clauses.toList()))
            }
        val solver = solverWithTheory(theory, libraries)
        val solution = solver.solve(Struct.parse(goal, solver.operators), 5.seconds).first()
        return when {
            solution.isYes ->
                solution.substitution.entries
                    .single { it.key.name == "L" }
                    .value
                    .toString()
            solution.isHalt -> "halt: ${solution.exception?.message}"
            else -> "no"
        }
    }

    @Test
    fun theAlternativesOfIsInAClauseOfTheTheory() {
        assertEquals("[2.5, theory]", solve("X is _ :- X = theory.\np(X) :- X is 1.5 + 1.", "findall(X, p(X), L)"))
    }

    @Test
    fun theAlternativesOfIsInAClauseWithAFrame() {
        assertEquals(
            "[2.5, theory]",
            solve("X is _ :- X = theory.\nq(X) :- X is 1.5 + 1, true.\np(X) :- q(X), true.", "findall(X, p(X), L)"),
        )
    }

    // (the registers of the clause, A in q(A), as they were when an alternative returns)
    @Test
    fun theAlternativesOfIsBehindAnInlineInstruction() {
        assertEquals(
            "[1.5, theory]",
            solve("X is _ :- X = theory.", "findall(R, p(1, R), L)", library = "p(A, R) :- R is A + 0.5, q(A).\nq(1)."),
        )
    }

    // (temporaries beyond the arguments of the builtin: D in a register above the third)
    @Test
    fun theRegistersOfTheClauseBeyondTheBuiltinsArguments() {
        assertEquals(
            "[1.5, theory]",
            solve(
                "X is _ :- X = theory.",
                "findall(R, p(1, 2, 3, 4, R), L)",
                library = "p(A, B, C, D, R) :- R is A + 0.5, q(A, B, C, D).\nq(1, 2, 3, 4).",
            ),
        )
    }
}
