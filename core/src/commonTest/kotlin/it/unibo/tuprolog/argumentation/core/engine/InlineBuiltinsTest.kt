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
        lt(X, Y) :- Y < X.
        isf(X, Y) :- Y is X + 1.5.
        isn(X, R) :- Y is X * 2.5, R = Y.
        cutf(X, R) :- member(R, [1, 2]), X > 0.5, !.
        ltcut(X, Y) :- X < Y, !.
        ltcut(_, _).
        a1(T, R) :- arg(1, T, R).
        a2(T, R) :- arg(2, T, R).
        a0(T, R) :- arg(0, T, R).
        av(T) :- arg(1, T, _).
        ac(T) :- arg(1, T, a).
        ak(T, R) :- arg(1, T, X), keep(X, X, Y, _), R = Y.
        ae(T, R) :- arg(1, T, X), arg(2, T, Y), R = p(X, Y).
        af(T, A, R) :- arg(1, T, X), q2(A, X, R).
        f3(T, N, A) :- functor(T, N, A).
        fn(T, N) :- functor(T, N, _).
        fa(T, A) :- functor(T, _, A).
        fc(T) :- functor(T, g, 2).
        fk(T, R) :- functor(T, N, A), keep(N, A, X, Y), R = p(X, Y).
        fb(N, A, R) :- functor(T, N, A), R = T.
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

    // What the goal answers, or the error it raises
    private fun outcome(goal: String): String {
        val solution = solver.solve(Struct.parse(goal, solver.operators)).first()
        return when {
            solution.isHalt -> "error " + solution.exception!!::class.simpleName
            solution.isYes ->
                solution.substitution.entries
                    .filter { it.key.name == "R" }
                    .joinToString { it.value.toString() }
                    .ifEmpty { "yes" }
            else -> "no"
        }
    }

    @Test
    fun argAsCalledAtRunTime() {
        val cases =
            listOf(
                "a1(g(1, 2), R)" to "arg(1, g(1, 2), R)",
                "a2(g(1, 2), R)" to "arg(2, g(1, 2), R)",
                "a2(g(1), R)" to "arg(2, g(1), R)",
                "a0(g(1), R)" to "arg(0, g(1), R)",
                "a1([x, y], R)" to "arg(1, [x, y], R)",
                "a2([x, y], R)" to "arg(2, [x, y], R)",
                "a1(a, R)" to "arg(1, a, R)",
                "a1(1, R)" to "arg(1, 1, R)",
                "a1(_, R)" to "arg(1, _, R)",
                "av(g(1))" to "arg(1, g(1), _)",
                "ac(g(a))" to "arg(1, g(a), a)",
                "ac(g(b))" to "arg(1, g(b), a)",
            )
        for ((inline, called) in cases) assertEquals(outcome("call(($called))"), outcome(inline), inline)
        assertEquals("1", answer("ak(g(1), R)"))
        assertEquals("p(1, 2)", answer("ae(g(1, 2), R)"))
        assertEquals("r(b, 1)", answer("af(g(1), b, R)"))
        assertEquals("v", answer("X = g(v), a1(X, R)"))
        assertTrue(holds("a1(g(X), Y), X = 1, Y == 1"))
    }

    @Test
    fun functorAsCalledAtRunTime() {
        val cases =
            listOf(
                "fn(g(1, 2), R)" to "functor(g(1, 2), R, _)",
                "fa(g(1, 2), R)" to "functor(g(1, 2), _, R)",
                "fn([x], R)" to "functor([x], R, _)",
                "fa([x], R)" to "functor([x], _, R)",
                "fn(a, R)" to "functor(a, R, _)",
                "fa(a, R)" to "functor(a, _, R)",
                "fn(1, R)" to "functor(1, R, _)",
                "fa(2.5, R)" to "functor(2.5, _, R)",
                "fn([], R)" to "functor([], R, _)",
                "fc(g(1, 2))" to "functor(g(1, 2), g, 2)",
                "fc(g(1))" to "functor(g(1), g, 2)",
                "f3(g(1), h, _)" to "functor(g(1), h, _)",
                "f3(g(1), _, a)" to "functor(g(1), _, a)",
                "f3(_, _, _)" to "functor(_, _, _)",
                "f3(_, g, a)" to "functor(_, g, a)",
                "f3(_, g, -1)" to "functor(_, g, -1)",
            )
        for ((inline, called) in cases) assertEquals(outcome("call(($called))"), outcome(inline), inline)
        assertEquals("p(g, 1)", answer("fk(g(a), R)"))
        assertTrue(holds("fc(T), T = g(1, 2)"))
        assertTrue(holds("fb(g, 2, T), T = g(1, 2)"))
        assertEquals("a", answer("fb(a, 0, R)"))
    }

    // Outside small integers the arithmetic goes through the builtin, the registers read as they were
    @Test
    fun arithmeticOnFloatsReadsItsOperands() {
        assertTrue(holds("lt(1.5, 0.5)"))
        assertFalse(holds("lt(0.5, 1.5)"))
        assertEquals("3.0", answer("isf(1.5, R)"))
        assertEquals("5.0", answer("isn(2, R)"))
        assertEquals("1", answer("cutf(1.5, R)"))
        assertEquals(1, solver.solve(Struct.parse("cutf(1.5, R)", solver.operators)).count { it.isYes })
        assertEquals(1, solver.solve(Struct.parse("ltcut(0.5, 1.5)", solver.operators)).count { it.isYes })
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
