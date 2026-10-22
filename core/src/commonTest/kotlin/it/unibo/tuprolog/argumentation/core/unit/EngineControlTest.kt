package it.unibo.tuprolog.argumentation.core.unit

import it.unibo.tuprolog.argumentation.core.TestingUtils.solverWithTheory
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import kotlin.test.Test
import kotlin.test.assertEquals

/** Control in the arg2p engine: cut in the goals of module calls and of call/1, negation, errors */
class EngineControlTest {
    private fun solve(goal: String): String {
        val solver = solverWithTheory("p(1).\np(2).\np(3).\nq(X) :- p(X), !.\nr(X) :- call((p(X), !)).\ns(X) :- p(X), call(!).")
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
    }

    @Test
    fun errorsOfCall() {
        assertEquals(true, solve("call(1)").startsWith("error"))
        assertEquals(true, solve("call(_)").startsWith("error"))
        assertEquals(true, solve("call((p(_), 1))").startsWith("error"))
        assertEquals("[]", solve("findall(X, catch(call(1), error(type_error(_, _), _), fail), L)"))
    }
}
