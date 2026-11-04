package it.unibo.tuprolog.argumentation.core.engine

import it.unibo.tuprolog.argumentation.core.TestingUtils.solverWithTheory
import it.unibo.tuprolog.argumentation.core.engine.vm.Program
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import kotlin.test.Ignore
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
        sum(X, Y, Z) :- Z is (X + Y) * 2 - X // 3 + Y mod 5 - (-X).
        modulo(X, Y, Z) :- Z is X mod Y.
        quotient(X, Y, Z) :- Z is X // Y.
        known(X, Y) :- 7 is X + Y.
        same(X, Y) :- X =:= Y.
        differ(X, Y) :- X =\= Y.
        gte(X, Y) :- X >= Y.
        lte(X, Y) :- X =< Y.
        gt(X, Y) :- X > Y.
        huge(X, R) :- Y is X * 1000000, Z is Y * 1000, R is Z + 1.
        neck(X, R) :- X > 0, !, R = positive.
        neck(X, R) :- X < 0, !, R = negative.
        neck(_, zero).
        neckall(X, R) :- mem(X, [1, 2, 3]), neck(X, R).
        direct(a, R) :- !, R = first.
        direct(_, other).
        late(X, R) :- mem(R, [1, 2]), X > 0, !.
        city(1, rome, it).
        city(2, milan, it).
        city(3, paris, fr).
        city(4, lyon, fr).
        city(5, berlin, de).
        city(6, bonn, de).
        city(7, madrid, es).
        city(8, seville, es).
        city(9, rome, us).
        city(10, _, none).
        pick(Name, R) :- city(R, Name, _).
        state(Code, R) :- city(R, _, Code).
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
            "sum(4, 9, R)",
            "sum(-4, 9, R)",
            "modulo(7, 3, R)",
            "modulo(-7, 3, R)",
            "modulo(7, -3, R)",
            "catch(modulo(1, 0, R), error(R, _), true)",
            "quotient(7, 2, R)",
            "quotient(-7, 2, R)",
            "catch(quotient(1, 0, R), error(R, _), true)",
            "catch(modulo(a, 1, R), error(R, _), true)",
            "catch(sum(_, 1, R), error(R, _), true)",
            "(known(3, 4), R = yes)",
            "(known(3, 3), R = yes)",
            "(same(2, 2), R = yes)",
            "(same(2, 3), R = yes)",
            "(same(2.0, 2), R = yes)",
            "(differ(2, 3), R = yes)",
            "(differ(2, 2), R = yes)",
            "(gte(2, 2), R = yes)",
            "(gte(1, 2), R = yes)",
            "(lte(2, 2), R = yes)",
            "(lte(3, 2), R = yes)",
            "(gt(3, 2), R = yes)",
            "(gt(2, 2), R = yes)",
            "(gt(2.5, 2), R = yes)",
            "huge(1, R)",
            "huge(-3, R)",
            "huge(1000, R)",
            "neck(1, R)",
            "neck(-1, R)",
            "neck(0, R)",
            "neckall(_, R)",
            "direct(a, R)",
            "direct(b, R)",
            "late(1, R)",
            "(late(0, R), R = no)",
            "pick(rome, R)",
            "pick(paris, R)",
            "pick(nowhere, R)",
            "state(de, R)",
            "state(it, R)",
            "state(none, R)",
            "state(xx, R)",
            "city(R, _, _)",
            "city(R, rome, it)",
            "city(3, R, _)",
        )

    @Ignore // (generated code is off: see Program.GENERATE_CODE)
    @Test
    fun answersDoNotChange() {
        val before = goals.map(::answers)
        for (goal in goals) solver.solve(Struct.parse("loop(${Program.HOT_CALLS + 1}, $goal)", solver.operators)).first()
        assertEquals(before, goals.map(::answers))
    }

    @Test
    fun anIndexOfALaterArgumentFollowsTheClauses() {
        val theory =
            """
            key(0, k0).
            key(1, k1).
            key(2, k2).
            key(3, k3).
            fill(N, N) :- !.
            fill(I, N) :- K is I mod 4, key(K, W), assertz(dyn(I, W)), J is I + 1, fill(J, N).
            asked(0) :- !.
            asked(N) :- dyn(_, k1), !, M is N - 1, asked(M).
            """.trimIndent()
        val made = "fill(1, 21), asked(100)"
        val goals =
            mapOf(
                "fill(1, 21), findall(X, dyn(X, k1), L)" to "[1, 5, 9, 13, 17]",
                "$made, findall(X, dyn(X, k1), L)" to "[1, 5, 9, 13, 17]",
                "$made, assertz(dyn(21, k1)), findall(X, dyn(X, k1), L)" to "[1, 5, 9, 13, 17, 21]",
                "$made, assertz(dyn(21, k1)), asked(100), findall(X, dyn(X, k1), L)" to "[1, 5, 9, 13, 17, 21]",
                "$made, retract(dyn(5, k1)), findall(X, dyn(X, k1), L)" to "[1, 9, 13, 17]",
                // (a clause with no key is a candidate for every key)
                "$made, assertz(dyn(22, _)), asked(100), findall(X, dyn(X, k1), L)" to "[1, 5, 9, 13, 17, 22]",
                "$made, assertz(dyn(22, _)), asked(100), findall(X, dyn(X, other), L)" to "[22]",
                "$made, findall(X, dyn(X, other), L)" to "[]",
            )
        for ((goal, expected) in goals) {
            val fresh = solverWithTheory(theory)
            assertEquals(
                expected,
                fresh
                    .solve(Struct.parse(goal, fresh.operators))
                    .first()
                    .substitution.entries
                    .firstOrNull { it.key.name == "L" }
                    ?.value
                    .toString(),
                goal,
            )
        }
    }

    @Test
    fun theHeapGrowsAndIsCollectedUnderCompiledCode() {
        solver.solve(Struct.parse("loop(${Program.HOT_CALLS + 1}, (count(3, L), len(L, _)))", solver.operators)).first()
        assertEquals("[200000]", answers("(count(200000, L0), len(L0, R))"))
    }
}
