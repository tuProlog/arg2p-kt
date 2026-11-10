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
    private fun bound(rules: String): String {
        val solver = solverWithTheory("")
        return solver
            .solve(Struct.parse("argumentSearch::boundByQuery($rules, Ids)", solver.operators))
            .first()
            .substitution
            .getByName("Ids")
            .toString()
    }

    @Test
    fun aVariableOfTheConclusionNoPremiseHas() {
        assertEquals("[]", bound("[[r, [q(X)], p(X)]]"))
        assertEquals("[s]", bound("[[s, [], p(Y)]]"))
        assertEquals("[]", bound("[[t, p(a)]]"))
        assertEquals("[u]", bound("[[u, p(Z)]]"))
    }

    @Test
    fun aConjunctionAsTheList() {
        assertEquals("[]", bound("[[r, (q(X), q(Y)), p(X, Y)]]"))
        assertEquals("[r]", bound("[[r, (q(X), q(Y)), p(X, Z)]]"))
    }

    @Test
    fun aCheckIsMetInItsOrder() {
        assertEquals("[]", bound("[[v, (a, prolog(X is 1), b(Y)), c(X, Y)]]"))
        assertEquals("[w]", bound("[[w, (prolog(Y is X), b(X)), c(Y)]]"))
        assertEquals("[]", bound("[[k, (b(X), prolog(Y is X + 1)), c(Y)]]"))
        assertEquals("[]", bound("[[k, (b(X), prolog((Y is X + 1, Z is Y + 1))), c(Z)]]"))
        assertEquals("[n]", bound("[[n, (prolog(X > 1), b(X)), c(X)]]"))
    }

    @Test
    fun contrariesAndConstraintsBindNothing() {
        assertEquals("[k]", bound("[[k, [{Q > 1}], p(Q)]]"))
        assertEquals("[m]", bound("[[m, [~(n(W))], p(W)]]"))
        assertEquals("[]", bound("[[k, (q(Q, [1, Y | T], 2.5), {Q > 1}), p(Q, Y, T)]]"))
    }

    // The premises as the conversion reads them: [] none, [X] the premise X, a list of more a single premise, one term
    @Test
    fun aListIsOnePremiseAsTheConversionReadsIt() {
        val module = LibraryTesting.parsed("r : [prolog(q(X)), p(X)] => c(X).\ns : [] => d(Y).\nt : [e(Z)] => f(Z).")
        assertTrue(module.holds("store::rule(r, [[prolog(q(X)), p(X)]], _)"))
        assertFalse(module.holds("argumentSearch::boundByQuery(r)"))
        assertEquals("[]", bound("[[r, [prolog(q(X)), p(X)], c(X)]]"))
        assertEquals("[]", bound("[[r, [{Q > 1}, p(Q)], c(Q)]]"))
        assertTrue(module.holds("store::rule(s, [], _)"))
        assertTrue(module.holds("argumentSearch::boundByQuery(s)"))
        assertTrue(module.holds("store::rule(t, [e(_)], _)"))
        assertFalse(module.holds("argumentSearch::boundByQuery(t)"))
    }

    @Test
    fun whatIsNotARuleIsLeftOut() {
        assertEquals("[b]", bound("[other, [a, b, c, d], [b, [], p(X)], f(x)]"))
    }
}
