package it.unibo.tuprolog.argumentation.core.system

import it.unibo.tuprolog.argumentation.Arg2pPrologSolverFactory
import it.unibo.tuprolog.argumentation.core.Arg2pSolverFactory
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.solve.MutableSolver
import it.unibo.tuprolog.solve.flags.FlagStore
import it.unibo.tuprolog.solve.flags.Unknown
import kotlin.test.Test
import kotlin.test.assertEquals

// The builtins of the machine (the tests of the module engine run them on the machine alone) answer alike with the
// libraries of arg2p loaded: from the theory, and inside the scope of a library (a module call, utils::Goal)
class EngineWithLibrariesTest {
    // Goals and the variable whose value is compared ("" for whether the goal holds)
    private val goals =
        listOf(
            "X is 2 + 3 * 4" to "X",
            "X is 2 ** 100" to "X",
            "X is 2 ** 100 - 2 ** 99" to "X",
            "X is 1.5 * 2" to "X",
            "X is 10 / 4" to "X",
            "X is 7 // 2" to "X",
            "X is -7 mod 3" to "X",
            "X is abs(-3) + sign(-2.5)" to "X",
            "X is 123456789012 * 1000" to "X",
            "catch(throw(e(1)), e(X), true)" to "X",
            "catch(X is foo + 1, error(type_error(T, _), _), true)" to "T",
            "catch(atom_length(_, _), error(E, _), true), E = instantiation_error" to "",
            "length(L, 2), L = [a, b]" to "",
            "length([a, b, c], N)" to "N",
            "findall(X, between(1, 4, X), L)" to "L",
            "findall(X-Y, (member(X, [1, 2]), member(Y, [a, b])), L)" to "L",
            "atom_codes(A, [0'a, 0'b])" to "A",
            "atom_chars(abc, L)" to "L",
            "T =.. [f, a, b]" to "T",
            "f(a, B) =.. L, length(L, 3)" to "",
            "copy_term(f(X, Y, X), f(A, B, C)), A == C, A \\== B" to "",
            "setof(X, member(X, [b, a, c, a]), L)" to "L",
            "bagof(X-Y, member(X-Y, [2-a, 1-b]), L)" to "L",
            "functor(T, g, 2), arg(1, T, x)" to "T",
            "atom_length(hello, N)" to "N",
            "sub_atom(hello, 1, 3, _, S)" to "S",
            "X = f(Y), Y = 1, X == f(1)" to "",
            "\\+ a = b" to "",
            "number_codes(N, [0'4, 0'2])" to "N",
            "number_chars(N, ['3', '.', '5'])" to "N",
            "f(a) @< f(b), 1.0 @< 1, a @< f(a)" to "",
            "X = [1, 2 | T], T = [3]" to "X",
            "once(member(X, [c, d]))" to "X",
            "\\+ (member(X, [1, 2, 3]), X =< 0)" to "",
            "(member(X, [1, 2, 3]), X > 1 -> Y = X ; Y = none)" to "Y",
            "findall(X, (member(X, [1, 2, 3, 4]), X mod 2 =:= 0), L)" to "L",
        )

    // The first solution of each goal, as text: the value, yes or no, or the error raised
    private fun answers(
        solver: MutableSolver,
        wrap: (String) -> String,
    ) = goals.map { (goal, variable) ->
        val solution = solver.solve(Struct.parse(wrap(goal), solver.operators)).first()
        when {
            solution.isHalt -> "halt: ${solution.exception?.message}"
            !solution.isYes -> "no"
            variable.isEmpty() -> "yes"
            // (the names of fresh variables differ from a solver to another)
            else ->
                solution.substitution
                    .getByName(variable)
                    .toString()
                    .replace(Regex("_[A-Za-z0-9_]+"), "_")
        }
    }

    private val machineAlone =
        Arg2pPrologSolverFactory.mutableSolverWithDefaultBuiltins(flags = FlagStore.DEFAULT.set(Unknown, Unknown.FAIL))

    @Test
    fun theBuiltinsAnswerAsOnTheMachineAlone() {
        val expected = answers(machineAlone) { it }
        assertEquals(expected, answers(Arg2pSolverFactory.default(theory = "")) { it })
    }

    @Test
    fun theBuiltinsAnswerAsOnTheMachineAloneInsideALibrary() {
        val expected = answers(machineAlone) { it }
        assertEquals(expected, answers(Arg2pSolverFactory.default(theory = "")) { "utils::($it)" })
    }
}
