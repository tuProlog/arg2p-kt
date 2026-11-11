package it.unibo.tuprolog.argumentation.core.libraries.framework.context

import it.unibo.tuprolog.argumentation.core.TestingUtils.solverWithTheory
import it.unibo.tuprolog.argumentation.engine.store.ClauseStore
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertTrue

/**
 * A lookup in a store unifies the heads of the records in place, on the heap of the machine: what it answers is what
 * the unification of a copy of each clause answers. The store of terms ([ClauseStore.solve], which renames and unifies
 * terms) gives the expected answers, for facts of all the shapes an index and a unification tell apart. (No goal builds
 * a cyclic term: the store of terms checks occurrences, the machine does not.)
 */
class StoreResolutionTest {
    private val clauses =
        listOf(
            "f(a, 1)",
            "f(b, 2.5)",
            "f(c, 10000000000)",
            "f(g(a), [1, 2])",
            "f(g(b), [1 | T])",
            "f(g(X), X)",
            "f(X, X)",
            "f(h(X, Y), k(Y, X))",
            "f([A, B | C], [C, B, A])",
            "f([], [])",
            "f(g(g(g(a))), deep)",
            "m(1)",
            "m(2)",
            "m(3)",
            "m(4)",
            "m(5)",
            "m(6)",
            "m(7)",
            "m(8)",
            "m(9)",
            "m(g(1))",
            "m(g(2))",
            "m(g(3))",
            "m(g(4))",
            "m(g(5))",
            "m(g(6))",
            "m(g(7))",
            "m(g(8))",
            "m(g(9))",
            "m(g(V))",
            "m(_)",
            "zero",
            "[x, y]",
            "[x, Z]",
        )

    private val goals =
        listOf(
            "f(a, X)",
            "f(X, 1)",
            "f(X, Y)",
            "f(b, 2.5)",
            "f(b, 2.6)",
            "f(c, 10000000000)",
            "f(c, 10000000001)",
            "f(g(a), X)",
            "f(g(Q), [1, 2])",
            "f(g(Q), [1, 2, 3])",
            "f(h(1, 2), X)",
            "f(h(1, Q), k(2, R))",
            "f(h(Q, Q), k(R, 1))",
            "f([1, 2, 3], X)",
            "f([1, 2], [Q, R, S])",
            "f([1], X)",
            "f([], X)",
            "f(g(g(g(a))), X)",
            "f(g(g(g(Q))), R)",
            "m(5)",
            "m(g(5))",
            "m(g(Q))",
            "m(h(1))",
            "m(Q)",
            "zero",
            "one",
            "[x, y]",
            "[x, Q]",
            "[Q, Q]",
        )

    private val setup get() = "contextReset, " + clauses.joinToString(", ") { "contextAssert($it)" }

    private fun normalized(term: String): String = term.replace(Regex("(?<![A-Za-z0-9_'])(_\\w*|[A-Z]\\w*_\\d+)"), "_")

    private fun onMachine(goal: String): List<String> {
        val solver = solverWithTheory("")
        val solution = solver.solve(Struct.parse("$setup, findall($goal, contextCheck($goal), L)", solver.operators)).first()
        assertTrue(solution.isYes, goal)
        val answers =
            solution.substitution.entries
                .first { it.key.name == "L" }
                .value
        return answers
            .castToList()
            .toList()
            .map { normalized(it.toString()) }
    }

    private fun onTerms(goal: String): List<String> {
        val solver = solverWithTheory("")
        val store = ClauseStore()
        clauses.forEach { store.assertA(Struct.parse(it, solver.operators)) }
        val parsed = Struct.parse(goal, solver.operators)
        return store.solve(parsed).map { normalized(parsed.apply(it).toString()) }.toList()
    }

    @Test
    fun answersAsTheUnificationOfTerms() {
        for (goal in goals) assertEquals(onTerms(goal), onMachine(goal), goal)
    }

    private fun holds(goal: String): Boolean {
        val solver = solverWithTheory("")
        return solver.solve(Struct.parse("$setup, $goal", solver.operators)).first().isYes
    }

    @Test
    fun absentAsNoAnswer() {
        for (goal in goals) {
            val expected = onTerms(goal).isEmpty()
            assertEquals(expected, holds("contextAbsent($goal)"), goal)
            assertEquals(expected, !holds("contextCheck($goal)"), goal)
        }
    }

    @Test
    fun absentLeavesNothingBound() {
        assertTrue(holds("X = 1, Y = X, \\+ contextAbsent(f(a, Y)), Y == 1"))
        assertTrue(holds("\\+ contextAbsent(f(a, Y)), var(Y)"))
        assertTrue(holds("contextAbsent(f(a, 2)), V = 1, V == 1"))
        assertTrue(holds("contextAssert(r(A) :- m(A)), \\+ contextAbsent(r(Q)), var(Q)"))
        assertTrue(holds("contextAssert(r2(A) :- missing(A)), contextAbsent(r2(_))"))
        assertTrue(holds("contextAssert(r3(A) :- body(A)), \\+ contextAbsent(clause(r3(Q), W)), var(Q), var(W)"))
        assertTrue(holds("contextAssert(r4(A) :- body(A)), contextAbsent(clause(r4(_), other))"))
    }

    @Test
    fun theVariablesOfAClauseAreTheSameAcrossItsHeadAndItsBody() {
        val solver = solverWithTheory("")
        val goal =
            "contextReset, contextAssert(p(1)), contextAssert(p(2)), contextAssert((q(X, g(X, Y)) :- p(X), p(Y))), " +
                "findall([A, B], contextCheck(q(A, g(B, 2))), L)"
        val solution = solver.solve(Struct.parse(goal, solver.operators)).first()
        assertEquals(
            "[[2, 2], [1, 1]]",
            solution.substitution.entries
                .first { it.key.name == "L" }
                .value
                .toString(),
        )
    }

    @Test
    fun clauseOfAFactIsTrue() {
        assertTrue(holds("contextAssert(c(1)), contextCheck(clause(c(1), B)), B == true"))
        assertTrue(holds("contextAssert(c(X, X)), contextCheck(clause(c(1, Q), true)), Q == 1"))
        assertFalse(holds("contextAssert(c(1)), contextCheck(clause(c(1), false))"))
    }

    @Test
    fun aClauseMustHaveACallableHead() {
        for (clause in listOf("1", "2.5", "_", "(1 :- a)", "(_ :- a)")) {
            val solver = solverWithTheory("")
            val solution =
                solver.solve(Struct.parse("catch(contextAssert($clause), _, Raised = yes), Raised == yes", solver.operators)).first()
            assertTrue(solution.isYes, clause)
        }
    }

    // Removing keeps the index of a predicate, or drops it when most clauses go: the lookups answer the same either way
    private fun afterRetract(
        facts: Int,
        retract: String,
        goal: String,
    ): String {
        val solver = solverWithTheory("")
        val goals =
            "contextReset, " + (1..facts).joinToString(", ") { "contextAssert(p($it, g($it), k))" } +
                ", contextCheck(p(1, _, _)), contextCheck(p(_, g(1), _)), contextCheck(p(_, _, k)), " +
                "$retract, findall(X, $goal, L)"
        return solver
            .solve(Struct.parse(goals, solver.operators))
            .first()
            .substitution.entries
            .first { it.key.name == "L" }
            .value
            .toString()
    }

    @Test
    fun lookupsAfterRemovingFewClauses() {
        val retract = "contextRetract(p(5, _, _))"
        assertEquals("[]", afterRetract(40, retract, "contextCheck(p(5, X, _))"))
        assertEquals("[]", afterRetract(40, retract, "contextCheck(p(X, g(5), _))"))
        assertEquals("[6]", afterRetract(40, retract, "contextCheck(p(X, g(6), _))"))
        assertEquals("[g(6)]", afterRetract(40, retract, "contextCheck(p(6, X, _))"))
        assertEquals(39, afterRetract(40, retract, "contextCheck(p(X, _, k))").split(",").size)
    }

    @Test
    fun lookupsAfterRemovingMostClauses() {
        val retract = "contextRetract(p(_, _, k))"
        assertEquals("[]", afterRetract(40, retract, "contextCheck(p(5, X, _))"))
        assertEquals("[]", afterRetract(40, retract, "contextCheck(p(X, g(6), _))"))
        assertEquals("[]", afterRetract(40, retract, "contextCheck(p(X, _, k))"))
        assertEquals(
            "[7]",
            afterRetract(40, "$retract, contextAssert(p(7, g(7), k))", "contextCheck(p(X, g(7), _))"),
        )
    }

    // Goals with all their arguments bound, on a predicate no argument alone narrows down: the index of the whole heads
    @Test
    fun lookupsOfWholeHeads() {
        fun holds(goal: String): Boolean {
            val solver = solverWithTheory("")
            val facts = (1..12).flatMap { a -> (1..12).map { b -> "contextAssert(e($a, $b))" } }.joinToString(", ")
            return solver.solve(Struct.parse("contextReset, $facts, contextAssert(e(X, X, open)), $goal", solver.operators)).first().isYes
        }
        assertTrue(holds("contextCheck(e(3, 4))"))
        assertFalse(holds("contextCheck(e(3, 13))"))
        assertTrue(holds("contextRetract(e(3, 5)), contextAbsent(e(3, 5)), contextCheck(e(3, 4))"))
        assertTrue(
            holds(
                "contextCheck(e(3, 4)), contextRetract(e(3, 4)), contextAbsent(e(3, 4)), contextAssert(e(3, 4)), contextCheck(e(3, 4))",
            ),
        )
        assertTrue(holds("contextCheck(e(3, 4)), contextAssert(e(20, 21)), contextCheck(e(20, 21))"))
        assertTrue(holds("contextAssert(e(Y, Y)), contextCheck(e(13, 13)), contextAbsent(e(13, 14))"))
        assertTrue(holds("contextCheck(e(3, 4)), contextAssert(e(_, 99)), contextCheck(e(3, 99)), contextAbsent(e(3, 98))"))
    }

    @Test
    fun aRemovalInABranchLeavesTheOthersAlone() {
        val solver = solverWithTheory("")
        val goals =
            "contextReset, " + (1..40).joinToString(", ") { "contextAssert(p($it))" } + ", contextCheck(p(1)), " +
                "contextActive(B0), contextBranch(B0, B1), contextRetract(p(3)), " +
                "findall(X, (member(X, [2, 3]), contextCheck(B1, p(X))), L1), " +
                "findall(X, (member(X, [2, 3]), contextCheck(B0, p(X))), L0), L = [L1, L0]"
        assertEquals(
            "[[2], [2, 3]]",
            solver
                .solve(Struct.parse(goals, solver.operators))
                .first()
                .substitution.entries
                .first { it.key.name == "L" }
                .value
                .toString(),
        )
    }

    @Test
    fun identifiersUpToTheNamesOfVariables() {
        assertTrue(holds("contextId(f(X, Y, X), I), contextId(f(A, B, A), I)"))
        assertFalse(holds("contextId(f(X, Y, X), I), contextId(f(A, B, B), I)"))
        assertFalse(holds("contextId(f(X, X, Y), I), contextId(f(A, B, B), I)"))
        assertTrue(holds("contextId([1, 2.5, a | T], I), contextId([1, 2.5, a | R], I)"))
        assertFalse(holds("contextId([1, 2], I), contextId('[|]'(1, 2), I)"))
        assertFalse(holds("contextId(a, I), contextId(a(1), I)"))
    }
}
