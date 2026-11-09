package it.unibo.tuprolog.argumentation.core.libraries.graph.structured.argument.argumentSearch

import it.unibo.tuprolog.argumentation.core.LibraryTesting
import it.unibo.tuprolog.argumentation.core.TestingUtils.solverWithTheory
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertTrue

/** The rules whose arguments a query could bind otherwise than the whole graph derives them */
class BoundByQueryTest {
    private fun bound(rules: String): Pair<String, String> {
        val solver = solverWithTheory("")
        val answer = { goal: String, name: String ->
            solver
                .solve(Struct.parse(goal, solver.operators))
                .first()
                .substitution
                .getByName(name)
                .toString()
        }
        return answer("argumentSearch::boundByQuery($rules, Ids)", "Ids") to
            answer("constraints::constrainedRules($rules, Constraints)", "Constraints")
    }

    @Test
    fun aVariableOfTheConclusionNoPremiseHas() {
        assertEquals("[]" to "no", bound("[[r, [q(X)], p(X)]]"))
        assertEquals("[s]" to "no", bound("[[s, [], p(Y)]]"))
        assertEquals("[]" to "no", bound("[[t, p(a)]]"))
        assertEquals("[u]" to "no", bound("[[u, p(Z)]]"))
    }

    @Test
    fun aConjunctionAsTheList() {
        assertEquals("[]" to "no", bound("[[r, (q(X), q(Y)), p(X, Y)]]"))
        assertEquals("[r]" to "no", bound("[[r, (q(X), q(Y)), p(X, Z)]]"))
    }

    @Test
    fun aCheckIsMetInItsOrder() {
        assertEquals("[]" to "no", bound("[[v, (a, prolog(X is 1), b(Y)), c(X, Y)]]"))
        assertEquals("[w]" to "no", bound("[[w, (prolog(Y is X), b(X)), c(Y)]]"))
        assertEquals("[]" to "no", bound("[[k, (b(X), prolog(Y is X + 1)), c(Y)]]"))
        assertEquals("[]" to "no", bound("[[k, (b(X), prolog((Y is X + 1, Z is Y + 1))), c(Z)]]"))
        assertEquals("[n]" to "no", bound("[[n, (prolog(X > 1), b(X)), c(X)]]"))
    }

    @Test
    fun contrariesAndConstraintsBindNothing() {
        assertEquals("[k]" to "yes", bound("[[k, [{Q > 1}], p(Q)]]"))
        assertEquals("[m]" to "no", bound("[[m, [~(n(W))], p(W)]]"))
        assertEquals("[]" to "yes", bound("[[k, (q(Q, [1, Y | T], 2.5), {Q > 1}), p(Q, Y, T)]]"))
    }

    // The premises as the conversion reads them: [] none, [X] the premise X, a list of more a single premise, one term
    @Test
    fun aListIsOnePremiseAsTheConversionReadsIt() {
        val module = LibraryTesting.parsed("r : [prolog(q(X)), p(X)] => c(X).\ns : [] => d(Y).\nt : [e(Z)] => f(Z).")
        assertTrue(module.holds("parser::rule(r, [[prolog(q(X)), p(X)]], _)"))
        assertFalse(module.holds("argumentSearch::boundByQuery(r)"))
        assertEquals("[]" to "no", bound("[[r, [prolog(q(X)), p(X)], c(X)]]"))
        assertEquals("[]" to "no", bound("[[r, [{Q > 1}, p(Q)], c(Q)]]"))
        assertTrue(module.holds("parser::rule(s, [], _)"))
        assertTrue(module.holds("argumentSearch::boundByQuery(s)"))
        assertTrue(module.holds("parser::rule(t, [e(_)], _)"))
        assertFalse(module.holds("argumentSearch::boundByQuery(t)"))
    }

    @Test
    fun whatIsNotARuleIsLeftOut() {
        assertEquals("[b]" to "no", bound("[other, [a, b, c, d], [b, [], p(X)], f(x)]"))
    }
}
