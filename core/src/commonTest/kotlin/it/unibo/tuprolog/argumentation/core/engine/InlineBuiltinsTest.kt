package it.unibo.tuprolog.argumentation.core.engine

import it.unibo.tuprolog.argumentation.core.TestingUtils.solverWithTheory
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertTrue

/** The builtins compiled inline answer as the same goals called at run time */
class InlineBuiltinsTest {
    private val theory =
        """
        keep(A, B, A, B).
        tv(X) :- var(X).
        tn(X) :- nonvar(X).
        ta(X) :- atom(X).
        tt(X) :- atomic(X).
        tc(X) :- callable(X).
        tp(X) :- compound(X).
        tf(X) :- float(X).
        ti(X) :- integer(X).
        tm(X) :- number(X).
        tfresh :- var(_).
        typ(X, Y, R) :- keep(X, Y, A, B), atomic(A), var(B), keep(A, B, R, _).
        eq(X, Y) :- X == Y.
        ne(X, Y) :- X \== Y.
        eqa(X) :- X == a.
        eqb(X) :- a == X.
        nea(X) :- X \== a.
        eqn(X) :- X == 1.
        eqf(X) :- X == 2.5.
        eqy(X, Y, R) :- keep(X, Y, A, B), keep(A, B, _, _), A == B, R = same.
        un1(X, Y) :- X = Y.
        un2(X, Y) :- Y = f(X).
        un3(X) :- X = [a, b | _].
        un4(R) :- X = g(Y), Y = 1, R = X.
        un5(R) :- X = Y, Y = 2, R = X.
        un6(R) :- X = g(Z), keep(X, Z, A, B), B = 3, R = A.
        un7(X, R) :- Y = h(X), keep(Y, Y, _, _), R = Y.
        un8(X, R) :- keep(X, X, A, _), A = k(V), keep(V, V, W, _), R = W.
        un9(X) :- _ = f(X).
        un10(R) :- _ = f(Z), Z = 4, R = Z.
        un11(X, Y) :- f(X) = Y.
        un12(R) :- a = R.
        un13(X, Y) :- X = X, Y = Y.
        un14(R) :- X = f(Y, Y), X = f(1, Z), R = Z.
        args(X, Y, R) :- Z = f(X), q2(Y, Z, R).
        q2(A, B, r(A, B)).
        moved(X, Y, R) :- X == a, Z = Y, q2(Z, X, R).
        ite(X, R) :- (var(X) -> R = free ; X == a -> R = isa ; R = other).
        naf(X) :- \+ X == a.
        dis(X, R) :- (X = a, R = 1 ; X = b, R = 2).
        cutted(X, R) :- X == a, !, R = 1.
        cutted(_, 2).
        below(X) :- atom(X).
        atom(1).
        over(X) :- X == 7.
        """.trimIndent()

    private val solver by lazy { solverWithTheory(theory) }

    private fun holds(goal: String): Boolean = solver.solve(Struct.parse(goal, solver.operators)).first().isYes

    private fun answer(goal: String): String =
        solver
            .solve(Struct.parse("$goal, L = R", solver.operators))
            .first()
            .substitution.entries
            .first { it.key.name == "L" }
            .value
            .toString()

    @Test
    fun typeTests() {
        for ((goal, expected) in listOf(
            "tv(_)" to true,
            "tv(a)" to false,
            "tn(a)" to true,
            "tn(_)" to false,
            "tt(a)" to true,
            "tt(1)" to true,
            "tt(2.5)" to true,
            "tt(f(a))" to false,
            "tc(a)" to true,
            "tc(f(a))" to true,
            "tc([a])" to true,
            "tc(1)" to false,
            "tp(f(a))" to true,
            "tp([a])" to true,
            "tp(a)" to false,
            "tf(2.5)" to true,
            "tf(1)" to false,
            "ti(1)" to true,
            "ti(10000000000)" to true,
            "ti(2.5)" to false,
            "tm(1)" to true,
            "tm(2.5)" to true,
            "tm(a)" to false,
            "tfresh" to true,
        )) {
            assertEquals(expected, holds(goal), goal)
        }
    }

    @Test
    fun typeTestsSeeBoundVariables() {
        assertTrue(holds("X = a, tn(X)"))
        assertFalse(holds("X = Y, Y = a, tv(X)"))
        assertTrue(holds("X = Y, tv(X)"))
        assertEquals("a", answer("typ(a, _, R)"))
    }

    @Test
    fun identity() {
        assertTrue(holds("eq(f(a), f(a))"))
        assertFalse(holds("eq(f(a), f(b))"))
        assertTrue(holds("eq(X, X)"))
        assertFalse(holds("eq(_, _)"))
        assertTrue(holds("X = Y, eq(X, Y)"))
        assertTrue(holds("ne(a, b)"))
        assertFalse(holds("ne(X, X)"))
        assertTrue(holds("eqa(a)"))
        assertTrue(holds("eqb(a)"))
        assertFalse(holds("eqa(b)"))
        assertFalse(holds("eqa(_)"))
        assertTrue(holds("nea(_)"))
        assertTrue(holds("X = a, eqa(X)"))
        assertFalse(holds("nea(a)"))
        assertTrue(holds("eqn(1)"))
        assertFalse(holds("eqn(1.0)"))
        assertTrue(holds("eqf(2.5)"))
        assertEquals("same", answer("eqy(a, a, R)"))
        assertFalse(holds("eqy(a, b, _)"))
    }

    @Test
    fun unification() {
        assertTrue(holds("un1(a, a)"))
        assertFalse(holds("un1(a, b)"))
        assertEquals("a", answer("un1(R, a)"))
        assertEquals("f(a)", answer("un2(a, R)"))
        assertEquals("a", answer("un2(R, f(a))"))
        assertFalse(holds("un2(a, g(a))"))
        assertTrue(holds("un3([a, b, c])"))
        assertFalse(holds("un3([a])"))
        assertEquals("g(1)", answer("un4(R)"))
        assertEquals("2", answer("un5(R)"))
        assertEquals("g(3)", answer("un6(R)"))
        assertEquals("h(a)", answer("un7(a, R)"))
        assertEquals("1", answer("un8(k(1), R)"))
        assertEquals("1", answer("X = k(1), un8(X, R)"))
        assertTrue(holds("un9(a)"))
        assertEquals("4", answer("un10(R)"))
        assertEquals("f(a)", answer("un11(a, R)"))
        assertEquals("a", answer("un12(R)"))
        assertTrue(holds("un13(a, b)"))
        assertEquals("1", answer("un14(R)"))
    }

    @Test
    fun registersStayWhereTheNextGoalReadsThem() {
        assertEquals("r(b, f(a))", answer("args(a, b, R)"))
        assertEquals("r(b, a)", answer("moved(a, b, R)"))
    }

    @Test
    fun control() {
        assertEquals("free", answer("ite(_, R)"))
        assertEquals("isa", answer("ite(a, R)"))
        assertEquals("other", answer("ite(b, R)"))
        assertTrue(holds("naf(b)"))
        assertFalse(holds("naf(a)"))
        assertEquals("1", answer("dis(a, R)"))
        assertEquals("2", answer("dis(b, R)"))
        assertEquals("1", answer("cutted(a, R)"))
        assertEquals("2", answer("cutted(b, R)"))
    }

    @Test
    fun aBuiltinTheTheoryDefinesKeepsItsClauses() {
        assertTrue(holds("below(1)"))
    }

    @Test
    fun theSameAsCalledAtRunTime() {
        for (goal in listOf("X = f(Y), Y = 1", "X = a, X == a", "atom(a)", "var(_)", "f(a) \\== f(b)")) {
            assertEquals(holds(goal), holds("call(($goal))"), goal)
        }
    }
}
