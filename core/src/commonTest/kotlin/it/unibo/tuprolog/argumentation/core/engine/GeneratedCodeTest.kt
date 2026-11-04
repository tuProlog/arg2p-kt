package it.unibo.tuprolog.argumentation.core.engine

import it.unibo.tuprolog.argumentation.core.TestingUtils.solverWithTheory
import it.unibo.tuprolog.argumentation.core.engine.vm.Program
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import kotlin.test.Test
import kotlin.test.assertEquals

/** Clauses called enough to be compiled by the platform answer as they did when they were not */
class GeneratedCodeTest {
    private val theory =
        """
        loop(0, _) :- !.
        loop(N, G) :- (call(G) -> true ; true), M is N - 1, loop(M, G).
        app([], L, L).
        app([H | T], L, [H | R]) :- app(T, L, R).
        nrev([], []).
        nrev([H | T], R) :- nrev(T, S), app(S, [H], R).
        tak(X, Y, Z, A) :- X =< Y, !, Z = A.
        tak(X, Y, Z, A) :-
            X1 is X - 1, Y1 is Y - 1, Z1 is Z - 1,
            tak(X1, Y, Z, A1), tak(Y1, Z, X, A2), tak(Z1, X, Y, A3), tak(A1, A2, A3, A).
        fact(0, 1) :- !.
        fact(N, F) :- M is N - 1, fact(M, G), F is N * G.
        mix(X, R) :- (var(X) -> R = free ; X == a -> R = isa ; R = other).
        dis(X, R) :- (X = a, R = 1 ; X = b, R = 2 ; R = 3).
        mem(X, [X | _]).
        mem(X, [_ | T]) :- mem(X, T).
        absent(X) :- \+ mem(X, [a, b]).
        shape(T, N, A, F) :- functor(T, N, A), arg(1, T, F).
        big(f(g(X), [Y | Z], h), r(X, Y, Z)).
        built(X, Y, f(g(X), [Y, X | Y], h(1, 2.5, a))).
        floats(X, Y) :- Y is X * 2.5 + 1.
        below(X, Y) :- Y < X.
        meta(G, yes) :- call(G).
        meta(_, no).
        check(X) :- X > 1, throw(oops(X)).
        check(_).
        caught(X, R) :- catch((check(X), R = none), oops(Y), R = caught(Y)).
        once1(X, R) :- mem(R, X), !.
        typed(X, R) :- integer(X), !, R = int.
        typed(X, R) :- atom(X), !, R = atom.
        typed(X, R) :- X \== f(a), compound(X), !, R = compound.
        typed(_, other).
        count(0, []) :- !.
        count(N, [N | T]) :- M is N - 1, count(M, T).
        len([], 0).
        len([_ | T], N) :- len(T, M), N is M + 1.
        """.trimIndent()

    private val solver by lazy { solverWithTheory(theory) }

    private fun answers(goal: String): String =
        solver
            .solve(Struct.parse("findall(R, $goal, L)", solver.operators))
            .first()
            .substitution.entries
            .first { it.key.name == "L" }
            .value
            .toString()

    private val goals =
        listOf(
            "nrev([1, 2, 3, 4, 5, 6, 7, 8], R)",
            "app(R, _, [1, 2, 3])",
            "tak(4, 2, 1, R)",
            "fact(6, R)",
            "mix(_, R)",
            "mix(a, R)",
            "mix(b, R)",
            "dis(a, R)",
            "dis(b, R)",
            "dis(_, R)",
            "(absent(c), R = yes)",
            "(absent(a), R = yes)",
            "(shape(g(1, 2), N, A, F), R = t(N, A, F))",
            "catch(shape(g, _, _, _), error(R, _), true)",
            "big(f(g(1), [2, 3], h), R)",
            "big(f(g(1), [], h), R)",
            "(built(1, b, f(g(1), [b, A | B], C)), R = t(A, B, C))",
            "floats(2, R)",
            "floats(1.5, R)",
            "(below(1.5, 0.5), R = yes)",
            "(below(1, 2), R = yes)",
            "meta(mem(1, [1, 2]), R)",
            "meta(fail, R)",
            "caught(1, R)",
            "caught(2, R)",
            "once1([a, b], R)",
            "typed(1, R)",
            "typed(a, R)",
            "typed(f(b), R)",
            "typed(f(a), R)",
            "typed(_, R)",
        )

    @Test
    fun answersDoNotChange() {
        val before = goals.map(::answers)
        for (goal in goals) solver.solve(Struct.parse("loop(${Program.HOT_CALLS + 1}, $goal)", solver.operators)).first()
        assertEquals(before, goals.map(::answers))
    }

    @Test
    fun theHeapGrowsAndIsCollectedUnderCompiledCode() {
        solver.solve(Struct.parse("loop(${Program.HOT_CALLS + 1}, (count(3, L), len(L, _)))", solver.operators)).first()
        assertEquals("[200000]", answers("(count(200000, L0), len(L0, R))"))
    }
}
