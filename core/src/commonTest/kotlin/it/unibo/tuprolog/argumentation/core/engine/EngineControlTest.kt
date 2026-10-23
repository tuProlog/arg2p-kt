package it.unibo.tuprolog.argumentation.core.engine

import it.unibo.tuprolog.argumentation.core.TestingUtils.solverWithTheory
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import kotlin.test.Test
import kotlin.test.assertEquals

/** Control in the arg2p engine: cut in the goals of module calls and of call/1, negation, errors, shared variables */
class EngineControlTest {
    private val theory =
        "p(1).\np(2).\np(3).\nq(X) :- p(X), !.\nr(X) :- call((p(X), !)).\ns(X) :- p(X), call(!).\n" +
            "same(Z, Z).\nchain(X, Y) :- same(X, Z), same(Z, Y).\npair(A, B, f(A, B))."

    private fun solve(goal: String): String {
        val solver = solverWithTheory(theory)
        val solution = solver.solve(Struct.parse(goal, solver.operators)).first()
        return when {
            solution.isYes ->
                solution.substitution.entries
                    .filter { it.key.name == "L" }
                    .joinToString { it.value.toString() }
            solution.isHalt -> "error: " + solution.exception!!.message
            else -> "no"
        }
    }

    @Test
    fun cutInAModuleCallIsLocalToItsGoal() {
        assertEquals("[1]", solve("findall(X, utils::(member(X, [1, 2, 3]), !), L)"))
        assertEquals("[1, 1]", solve("findall(X, (member(_, [a, b]), utils::(member(X, [1, 2, 3]), !)), L)"))
    }

    @Test
    fun cutInCallIsLocalToItsGoal() {
        assertEquals("[1]", solve("findall(X, call((p(X), !)), L)"))
        assertEquals("[1]", solve("findall(X, r(X), L)"))
        assertEquals("[1, 2, 3]", solve("findall(X, s(X), L)"))
        assertEquals("[1, 1]", solve("findall(X, (member(_, [a, b]), call((p(X), !))), L)"))
        assertEquals("[1]", solve("findall(X, q(X), L)"))
    }

    @Test
    fun negation() {
        assertEquals("[2, 3]", solve("findall(X, (p(X), \\+ X = 1), L)"))
        assertEquals("[]", solve("findall(X, (p(X), \\+ p(X)), L)"))
        // a cut in the negated goal is local to it, and its bindings are undone
        assertEquals("yes", solve("\\+ (!, fail), L = yes"))
        assertEquals("[2, 3]", solve("findall(X, (p(X), \\+ (p(Y), !, Y = X)), L)"))
        assertEquals("yes", solve("\\+ \\+ X = 1, var(X), L = yes"))
        assertEquals("[2]", solve("findall(X, (p(X), \\+ \\+ X = 2), L)"))
        assertEquals("no", solve("\\+ (p(X), X > 2)"))
        assertEquals("yes", solve("utils::(\\+ fail), L = yes"))
        assertEquals(true, solve("\\+ 1").startsWith("error"))
        assertEquals(true, solve("\\+ _").startsWith("error"))
        assertEquals("[]", solve("findall(X, catch(\\+ 1, error(type_error(_, _), _), fail), L)"))
    }

    @Test
    fun errorsOfCall() {
        assertEquals(true, solve("call(1)").startsWith("error"))
        assertEquals(true, solve("call(_)").startsWith("error"))
        assertEquals(true, solve("call((p(_), 1))").startsWith("error"))
        assertEquals("[]", solve("findall(X, catch(call(1), error(type_error(_, _), _), fail), L)"))
    }

    // The variables of a clause are bound to the ones of the goal: those the clause makes equal stay equal
    @Test
    fun variablesSharedThroughClauses() {
        assertEquals("yes", solve("same(X, Y), (X == Y -> L = yes ; L = no)"))
        assertEquals("yes", solve("chain(X, Y), (X == Y -> L = yes ; L = no)"))
        assertEquals("[b, b]", solve("chain(X, Y), Y = b, L = [X, Y]"))
        assertEquals("f(1, 2)", solve("pair(X, Y, T), X = 1, Y = 2, L = T"))
        assertEquals("[f(1, 2)]", solve("findall(T, (pair(X, Y, T), member(X-Y, [1-2])), L)"))
    }
}
