package it.unibo.tuprolog.argumentation.core.context

import it.unibo.tuprolog.argumentation.core.TestingUtils.solverWithTheory
import it.unibo.tuprolog.argumentation.engine.vm.ClauseStore
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

    private val setup get() = "context_reset, " + clauses.joinToString(", ") { "context_assert($it)" }

    private fun normalized(term: String): String = term.replace(Regex("(?<![A-Za-z0-9_'])(_\\w*|[A-Z]\\w*_\\d+)"), "_")

    private fun onMachine(goal: String): List<String> {
        val solver = solverWithTheory("")
        val solution = solver.solve(Struct.parse("$setup, findall($goal, context_check($goal), L)", solver.operators)).first()
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
            assertEquals(expected, holds("context_absent($goal)"), goal)
            assertEquals(expected, !holds("context_check($goal)"), goal)
        }
    }

    @Test
    fun absentLeavesNothingBound() {
        assertTrue(holds("X = 1, Y = X, \\+ context_absent(f(a, Y)), Y == 1"))
        assertTrue(holds("\\+ context_absent(f(a, Y)), var(Y)"))
        assertTrue(holds("context_absent(f(a, 2)), V = 1, V == 1"))
        assertTrue(holds("context_assert(r(A) :- m(A)), \\+ context_absent(r(Q)), var(Q)"))
        assertTrue(holds("context_assert(r2(A) :- missing(A)), context_absent(r2(_))"))
        assertTrue(holds("context_assert(r3(A) :- body(A)), \\+ context_absent(clause(r3(Q), W)), var(Q), var(W)"))
        assertTrue(holds("context_assert(r4(A) :- body(A)), context_absent(clause(r4(_), other))"))
    }

    @Test
    fun theVariablesOfAClauseAreTheSameAcrossItsHeadAndItsBody() {
        val solver = solverWithTheory("")
        val goal =
            "context_reset, context_assert(p(1)), context_assert(p(2)), context_assert((q(X, g(X, Y)) :- p(X), p(Y))), " +
                "findall([A, B], context_check(q(A, g(B, 2))), L)"
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
        assertTrue(holds("context_assert(c(1)), context_check(clause(c(1), B)), B == true"))
        assertTrue(holds("context_assert(c(X, X)), context_check(clause(c(1, Q), true)), Q == 1"))
        assertFalse(holds("context_assert(c(1)), context_check(clause(c(1), false))"))
    }

    @Test
    fun aClauseMustHaveACallableHead() {
        for (clause in listOf("1", "2.5", "_", "(1 :- a)", "(_ :- a)")) {
            val solver = solverWithTheory("")
            val solution =
                solver.solve(Struct.parse("catch(context_assert($clause), _, Raised = yes), Raised == yes", solver.operators)).first()
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
            "context_reset, " + (1..facts).joinToString(", ") { "context_assert(p($it, g($it), k))" } +
                ", context_check(p(1, _, _)), context_check(p(_, g(1), _)), context_check(p(_, _, k)), " +
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
        val retract = "context_retract(p(5, _, _))"
        assertEquals("[]", afterRetract(40, retract, "context_check(p(5, X, _))"))
        assertEquals("[]", afterRetract(40, retract, "context_check(p(X, g(5), _))"))
        assertEquals("[6]", afterRetract(40, retract, "context_check(p(X, g(6), _))"))
        assertEquals("[g(6)]", afterRetract(40, retract, "context_check(p(6, X, _))"))
        assertEquals(39, afterRetract(40, retract, "context_check(p(X, _, k))").split(",").size)
    }

    @Test
    fun lookupsAfterRemovingMostClauses() {
        val retract = "context_retract(p(_, _, k))"
        assertEquals("[]", afterRetract(40, retract, "context_check(p(5, X, _))"))
        assertEquals("[]", afterRetract(40, retract, "context_check(p(X, g(6), _))"))
        assertEquals("[]", afterRetract(40, retract, "context_check(p(X, _, k))"))
        assertEquals(
            "[7]",
            afterRetract(40, "$retract, context_assert(p(7, g(7), k))", "context_check(p(X, g(7), _))"),
        )
    }

    // Goals with all their arguments bound, on a predicate no argument alone narrows down: the index of the whole heads
    @Test
    fun lookupsOfWholeHeads() {
        fun holds(goal: String): Boolean {
            val solver = solverWithTheory("")
            val facts = (1..12).flatMap { a -> (1..12).map { b -> "context_assert(e($a, $b))" } }.joinToString(", ")
            return solver.solve(Struct.parse("context_reset, $facts, context_assert(e(X, X, open)), $goal", solver.operators)).first().isYes
        }
        assertTrue(holds("context_check(e(3, 4))"))
        assertFalse(holds("context_check(e(3, 13))"))
        assertTrue(holds("context_retract(e(3, 5)), context_absent(e(3, 5)), context_check(e(3, 4))"))
        assertTrue(
            holds(
                "context_check(e(3, 4)), context_retract(e(3, 4)), context_absent(e(3, 4)), context_assert(e(3, 4)), context_check(e(3, 4))",
            ),
        )
        assertTrue(holds("context_check(e(3, 4)), context_assert(e(20, 21)), context_check(e(20, 21))"))
        assertTrue(holds("context_assert(e(Y, Y)), context_check(e(13, 13)), context_absent(e(13, 14))"))
        assertTrue(holds("context_check(e(3, 4)), context_assert(e(_, 99)), context_check(e(3, 99)), context_absent(e(3, 98))"))
    }

    @Test
    fun aRemovalInABranchLeavesTheOthersAlone() {
        val solver = solverWithTheory("")
        val goals =
            "context_reset, " + (1..40).joinToString(", ") { "context_assert(p($it))" } + ", context_check(p(1)), " +
                "context_active(B0), context_branch(B0, B1), context_retract(p(3)), " +
                "findall(X, (member(X, [2, 3]), context_check(B1, p(X))), L1), " +
                "findall(X, (member(X, [2, 3]), context_check(B0, p(X))), L0), L = [L1, L0]"
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
        assertTrue(holds("context_id(f(X, Y, X), I), context_id(f(A, B, A), I)"))
        assertFalse(holds("context_id(f(X, Y, X), I), context_id(f(A, B, B), I)"))
        assertFalse(holds("context_id(f(X, X, Y), I), context_id(f(A, B, B), I)"))
        assertTrue(holds("context_id([1, 2.5, a | T], I), context_id([1, 2.5, a | R], I)"))
        assertFalse(holds("context_id([1, 2], I), context_id('[|]'(1, 2), I)"))
        assertFalse(holds("context_id(a, I), context_id(a(1), I)"))
    }
}
