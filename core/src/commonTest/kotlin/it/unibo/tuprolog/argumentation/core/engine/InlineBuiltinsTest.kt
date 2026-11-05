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
        viaCall(G, R) :- id(a, R0), call(G), id(R0, R).
        viaLast(G) :- id(a, _), call(G).
        viaOnly(G) :- call(G).
        viaTemporary(G) :- id(G, H), call(H).
        cutStaysIn(R) :- member(R, [1, 2]), id(!, G), call(G).
        cutAfter(R) :- member(R, [1, 2]), id(true, G), call(G), !.
        notCalled(G) :- \+ call(G).
        same0(0) :- !.
        same0(_).
        nested(f(g(A, B), h(C, D)), k(l(A, C), m(B, D))).
        builds(A, B, C, D, R) :- id(k(l(A, C), m(B, D)), R).
        cutThenCall(X, Y, Z) :- !, id(X, Y), id(Y, Z).
        threeSlots(A, B, C) :- id(A, X), threeChecked(X, B, C).
        threeChecked(1, 2, 3).
        twoSlots(A, B) :- id(A, X), id(B, Y), twoChecked(X, Y), id(A, A).
        twoLast(A, B) :- id(A, X), id(B, Y), twoChecked(X, Y).
        twoChecked(1, 2).
        id(X, X).
        moved(T, C) :- compound(T), !, functor(T, _, A), movedArg(A, T, C).
        moved(T, T).
        movedArg(A, T, C) :- arg(A, T, C).
        same(X, X, 1) :- !.
        same(_, _, 0).
        same8(a, _, 1).
        same8(b, _, 2).
        same8(c, _, 3).
        same8(d, _, 4).
        same8(e, _, 5).
        same8(f, _, 6).
        same8(g, _, 7).
        same8(X, X, 8) :- !.
        same8(_, _, 9).
        commit(f(X), Y, g(X, Y)) :- !.
        commit(_, _, none).
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
        fbn(N, R) :- functor(T, N, 2), R = T.
        stale(R) :- grow(300000, L), stq(N), X = f(N, N), str(X, N), R = X, L = [_ | _].
        stq(1).
        stq(2) :- grow(600000, _).
        str(_, 2).
        grow(0, []) :- !.
        grow(N, [N | T]) :- M is N - 1, grow(M, T).
        ov(X, Y, R) :- R is X * Y.
        ova(X, Y, R) :- R is X + Y.
        ovs(X, Y, R) :- R is X - Y.
        ovn(X, R) :- R is -X.
        ovc(X, Y) :- X > Y.
        ovd(X, R) :- Y is X * X, R is Y // X.
        sl(X, [Y | T], R) :- Y is X + 1, q2(Y, T, R).
        fl(X, R) :- Y is X + 1, q2(Y, X, R).
        fo(X, R) :- Y is X * X, q2(Y, X, R).
        fg(0, R, R) :- !.
        fg(N, S, R) :- S1 is S + N, M is N - 1, fg(M, S1, R).
        manycps(0).
        manycps(N) :- N > 0, M is N - 1, manycps(M).
        manycps(_).
        tp1(f(g(X), h(Y, X)), X, Y).
        tp2(X, f(g(X), [X, Y | Z], h(k(Y))), Z).
        tp3([f(X), g(X, _) | T], X, T).
        tp4(f(g(h(i(X)))), X).
        tp5(a, f(g(Y), Y), Y).
        tp6(X, R) :- X = f(g(A), k(A, b)), R = A.
        tp7(f(_, g(_), _)).
        tp8(f(g(X, Y), Y), X, R) :- R = t(X, Y).
        cm1(f(A, B), C, g(A, h(B, C))) :- !.
        cm1(_, _, other).
        cm2(f(A), B, g(A)) :- !, B = done.
        cm2(_, B, none) :- B = second.
        cm3(f(A), A) :- !.
        cm3(_, other).
        cm4(f(A), g(A)) :- !.
        cm4(f(_), g(z)).
        cm4(_, last).
        cm5(a, _) :- !.
        cm5(_, b).
        cm6(f(A, A), R) :- !, R = same.
        cm6(_, other).
        rot(A, B, C, R) :- q3(B, C, A, R).
        rot2(A, B, C, R) :- q3(C, A, B, R).
        swp(A, B, C, R) :- q3(B, A, C, R).
        two(A, B, R) :- q3(B, B, A, R).
        mix(A, B, R) :- q3(f(A), B, B, R).
        gua(A, B, C, R) :- A > 0, q3(C, A, B, R).
        typ2(A, B, R) :- atomic(A), q3(B, A, c, R).
        hed(f(A), B, C, R) :- q3(C, B, A, R).
        nws(A, B, R) :- C is A + 1, q3(C, A, B, R).
        lst(A, B, C, R) :- q3l(C, A, B, R).
        q3l(A, B, C, l(A, B, C)).
        lg(X, Y, R) :- X < Y, !, R = lt.
        lg(X, Y, R) :- X > Y, !, R = gt.
        lg(_, _, eq).
        tg(X, R) :- integer(X), !, R = int.
        tg(X, R) :- X == a, !, R = isa.
        tg(X, R) :- var(X), !, R = free.
        tg(_, other).
        gm(X, Y, A, B, R) :- X < Y, !, A = B, R = yes.
        gm(_, _, A, A, no).
        in1(f(a, _), one).
        in1(f(b, _), two).
        in1(f(_, c), three).
        in1(g(a), four).
        in2([a | _], one).
        in2([b | _], two).
        in2([], three).
        st1(f(X), X).
        st3(g(A, b, f(A)), A).
        st4(h(_, X, _, X), X).
        st5(X, g(X, Y, Y), Y).
        st6(k(l(m(X))), X).
        ar1(I, T, R) :- arg(I, T, X), keep(X, X, _, _), R = X.
        ar2(I, T, R) :- arg(I, T, X), arg(I, T, Y), keep(X, Y, A, B), R = p(A, B).
        ar3(T, R) :- arg(I, T, X), keep(I, X, A, B), R = p(A, B).
        sa1(A, B, C, R) :- q3(A, B, C, D), R = D.
        sa2(A, B, R) :- q3(B, A, A, D), R = t(A, B, D).
        sa3(A, B, R) :- C is A + 1, q3(C, B, B, D), R = t(A, B, C, D).
        q3(A, B, C, r(A, B, C)).
        co1(X, R) :- (X > 1 -> R = big ; X > 0 -> R = small ; R = none).
        co2(X, R) :- (X == a, R = isa ; X == b, R = isb ; R = other).
        co3(X, R) :- (keep(X, X, A, _), A > 1 -> R = big ; R = other).
        co4(X, R) :- (X = f(Y), Y > 1 -> R = Y ; X = f(Y) -> R = low(Y) ; R = no).
        co5(X, R) :- \+ X > 1, R = notbig.
        co5(_, alternative).
        co6(X) :- \+ \+ X = a, var(X).
        co7(R) :- (mem2(X, [1, 2, 3]), X > 1, ! ; X = 0), R = X.
        co7(no).
        co8(X, R) :- ((X > 1, !) -> R = first ; R = second).
        co8(_, third).
        co9(X, R) :- (X > 1 -> (X > 2 -> R = a ; R = b) ; (X > 0 -> R = c ; R = d)).
        co10(X, R) :- (X = 1 ; X = 2), Y is X + 1, (Y > 2 -> R = Y ; R = low).
        co11(X, R) :- (X < 1.5 -> R = low ; R = high).
        co12(X, R) :- catch((X > 1 -> R = big ; R = small), error(_, _), R = e).
        mem2(X, [X | _]).
        mem2(X, [_ | T]) :- mem2(X, T).
        cvv(X, Y) :- X < Y.
        cvy(X, Y) :- keep(X, Y, A, B), keep(A, B, C, D), C =< D.
        cvc(X) :- X >= 3.
        ccv(X) :- 3 >= X.
        ccl(X) :- 3 < X.
        ceq(X) :- X =:= 3.
        cne(X) :- 3 =\= X.
        inc(X, R) :- R is X + 1.
        dec(X, R) :- R is X - 1.
        cin(X, R) :- R is 2 + X.
        iny(X, R) :- Y is X + 1, keep(Y, Y, A, _), Z is A + 1, keep(Z, Z, R, _).
        ink(X) :- 5 is X + 1.
        inv(X, R) :- keep(X, X, A, _), R is A + 1.
        inn(X, Y) :- X is Y + 1.
        dl1(f(X), X, a).
        dl1(f(X), Y, b) :- X == Y.
        dl1(_, _, c).
        dl2(X, R) :- X > 1, !, R = big.
        dl2(X, R) :- X > 0, !, R = small.
        dl2(_, none).
        dl3(X, R) :- X > 1, keep(X, X, A, _), R = A.
        dl3(X, neg) :- X < 0.
        dl4(V, X) :- V = bound, X > 1.
        dl4(V, _) :- var(V).
        dl6(1).
        dl6(2).
        dl6(3).
        dl7(X) :- dl6(X), X > 1, !.
        dl8([H | T], T, H, one).
        dl8([H | T], H, T, two).
        dl9(X, R) :- dl6(X), dl2(X, R).
        dl10(X, Y) :- X = f(Y), Y > 1.
        dl10(X, Y) :- X = g(Y), Y > 0.
        dl11(X, R) :- (X > 1 -> R = big ; R = other).
        dl11(_, alternative).
        leaf(A, B, C, D) :- A =< B, !, C = D.
        leaf(_, _, _, far).
        leafn(A, B, A, B) :- nonvar(A).
        within(f(X), X).
        withinlist([X | _], X).
        withintwo(f(X), g(Y), Y, X).
        afterit(X, f(X)).
        stays(Y, [X | Xs], D, R) :- Y =\= X + D, D1 is D + 1, q2(Xs, D1, R).
        readafter(D, R) :- D1 is D + 1, E is D * 2, q2(D1, E, R).
        stays2(A, B, R) :- integer(A), C is A + 1, q2(B, C, R).
        av1(I, T, R) :- arg(I, T, R).
        av2(I, T, R) :- arg(I, T, X), q2(X, I, R).
        av3(T, R) :- arg(I, T, X), q2(I, X, R).
        av4(I, T, X) :- arg(I, T, X), X == a.
        av5(I, T, R) :- keep(I, T, A, B), arg(A, B, X), keep(X, X, R, _).
        av6(I, T, f(Y)) :- arg(I, T, f(Y)).
        kept(N, R) :- W = g(N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N), functor(_, a, 0), R = W.
        churn(0) :- !.
        churn(N) :- kept(N, R), arg(1, R, A), arg(200, R, B), A == N, B == N, M is N - 1, churn(M).
        """.trimIndent()

    private val solver by lazy { solverWithTheory(theory) }

    private fun all(
        template: String,
        goal: String,
    ): String =
        solver
            .solve(Struct.parse("findall($template, $goal, L)", solver.operators))
            .first()
            .substitution.entries
            .first { it.key.name == "L" }
            .value
            .toString()

    @Test
    fun argumentsLeftInTheirRegisters() {
        assertEquals("[1]", all("R", "leaf(1, 2, R, 1)"))
        assertEquals("[far]", all("R", "leaf(2, 1, _, R)"))
        assertEquals("[r([b], 3)]", all("R", "stays(1, [9, b], 2, R)"))
        assertEquals("[r(4, 6)]", all("R", "readafter(3, R)"))
        assertEquals("[r(b, 4)]", all("R", "stays2(3, b, R)"))
        assertEquals("[x]", all("B", "(leafn(a, B, a, x), B = x)"))
        assertEquals("[yes]", all("Y", "(within(f(a), a), Y = yes)"))
        assertEquals("[]", all("Y", "(within(f(a), b), Y = yes)"))
        assertEquals("[a]", all("R", "within(f(a), R)"))
        assertEquals("[]", all("Y", "(withinlist([a], b), Y = yes)"))
        assertEquals("[a]", all("R", "withinlist([a], R)"))
        assertEquals("[]", all("Y", "(withintwo(f(1), g(2), 3, 1), Y = yes)"))
        assertEquals("[yes]", all("Y", "(withintwo(f(1), g(2), 2, 1), Y = yes)"))
        assertEquals("[]", all("Y", "(afterit(a, f(b)), Y = yes)"))
        assertEquals("[a]", all("R", "afterit(a, f(R))"))
    }

    @Test
    fun arithmeticInOneInstruction() {
        val big = "3000000000"
        val cases =
            listOf(
                "cvv(1, 2)" to true,
                "cvv(2, 2)" to false,
                "cvv(1.5, 2)" to true,
                "cvv(2, 1.5)" to false,
                "cvv(1, $big)" to true,
                "cvv($big, 1)" to false,
                "cvy(2, 2)" to true,
                "cvy(3, 2)" to false,
                "cvc(3)" to true,
                "cvc(2)" to false,
                "cvc(3.5)" to true,
                "cvc($big)" to true,
                "ccv(3)" to true,
                "ccv(4)" to false,
                "ccv(2.5)" to true,
                "ccv($big)" to false,
                "ccl(4)" to true,
                "ccl(3)" to false,
                "ceq(3)" to true,
                "ceq(4)" to false,
                "ceq(3.0)" to true,
                "cne(4)" to true,
                "cne(3)" to false,
                "ink(4)" to true,
                "ink(5)" to false,
                "inn(5, 4)" to true,
                "inn(6, 4)" to false,
            )
        for ((goal, expected) in cases) assertEquals(expected, holds(goal), goal)
        assertEquals("[3]", all("R", "inc(2, R)"))
        assertEquals("[1]", all("R", "dec(2, R)"))
        assertEquals("[4]", all("R", "cin(2, R)"))
        assertEquals("[2.5]", all("R", "inc(1.5, R)"))
        assertEquals("[4]", all("R", "iny(2, R)"))
        assertEquals("[3]", all("R", "inv(2, R)"))
        assertEquals("[3000000001]", all("R", "inc($big, R)"))
        assertEquals("[2147483648]", all("R", "inc(2147483647, R)"))
        assertEquals("[-2147483649]", all("R", "dec(-2147483648, R)"))
        assertEquals("[e]", all("R", "catch(inc(_, R), error(_, _), R = e)"))
        assertEquals("[e]", all("R", "catch((inc(a, X), R = X), error(_, _), R = e)"))
        assertEquals("[e]", all("R", "catch((cvv(_, 1), R = n), error(_, _), R = e)"))
        assertEquals("[e]", all("R", "catch((cvc(a), R = n), error(_, _), R = e)"))
    }

    @Test
    fun aCollectionFindsAFrameSlotLeftBehindByARetry() {
        assertEquals("[f(2, 2)]", all("R", "stale(R)"))
    }

    @Test
    fun integersBeyondTheWordsOfTheMachine() {
        val max = "9223372036854775807"
        for ((goal, expected) in listOf(
            "ov(3037000500, 3037000500, R)" to "[9223372037000250000]",
            "ov(4294967296, 4294967296, R)" to "[18446744073709551616]",
            "ov(-4294967296, 4294967296, R)" to "[-18446744073709551616]",
            "ova($max, 1, R)" to "[9223372036854775808]",
            "ovs(-$max, 2, R)" to "[-9223372036854775809]",
            "ovn(-9223372036854775808, R)" to "[9223372036854775808]",
            "ovd(4294967296, R)" to "[4294967296]",
            "ova(18446744073709551616, 1, R)" to "[18446744073709551617]",
            "ovs(18446744073709551616, 18446744073709551616, R)" to "[0]",
            "ova(1, 2, R)" to "[3]",
            "ov(3000000000, 2, R)" to "[6000000000]",
        )) {
            assertEquals(expected, all("R", goal), goal)
        }
        for ((goal, expected) in listOf(
            "ovc(18446744073709551616, $max)" to true,
            "ovc($max, 18446744073709551616)" to false,
            "ovc(18446744073709551617, 18446744073709551616)" to true,
            "18446744073709551616 == 18446744073709551616" to true,
            "18446744073709551616 == 18446744073709551617" to false,
            "18446744073709551616 = 18446744073709551616" to true,
            "integer(18446744073709551616)" to true,
            "atomic(18446744073709551616)" to true,
        )) {
            assertEquals(expected, holds(goal), goal)
        }
    }

    @Test
    fun structuresOfAnyArity() {
        for ((goal, expected) in listOf(
            "st1(f(1), R)" to "[1]",
            "st1(g(1), R)" to "[]",
            "(st1(X, 1), X = f(R))" to "[1]",
            "st3(g(1, b, f(1)), R)" to "[1]",
            "st3(g(1, c, f(1)), R)" to "[]",
            "st3(g(1, b, f(2)), R)" to "[]",
            "(st3(X, 1), X = g(A, B, C), R = t(A, B, C))" to "[t(1, b, f(1))]",
            "st4(h(0, 1, 2, 1), R)" to "[1]",
            "st4(h(0, 1, 2, 3), R)" to "[]",
            "(st4(X, 1), arg(4, X, R))" to "[1]",
            "st5(1, g(1, 2, 2), R)" to "[2]",
            "st5(1, g(2, 2, 2), R)" to "[]",
            "st5(1, g(1, 2, 3), R)" to "[]",
            "(st5(1, X, 2), X = g(A, B, C), R = t(A, B, C))" to "[t(1, 2, 2)]",
            "st6(k(l(m(1))), R)" to "[1]",
            "st6(k(l(n(1))), R)" to "[]",
            "(st6(X, 1), X = k(l(m(R))))" to "[1]",
        )) {
            assertEquals(expected, all("R", goal), goal)
        }
    }

    @Test
    fun argGivingASlotAndArgumentsLeftInPlace() {
        for ((goal, expected) in listOf(
            "ar1(1, f(a, b), R)" to "[a]",
            "ar1(3, f(a, b), R)" to "[]",
            "ar1(0, f(a, b), R)" to "[]",
            "ar1(_, f(a, b), R)" to "[a, b]",
            "ar1(1, [a, b], R)" to "[a]",
            "catch(ar1(1, a, _), error(R, _), true)" to "[type_error(compound, a)]",
            "ar2(2, f(a, b), R)" to "[p(b, b)]",
            "ar2(_, f(a, b), R)" to "[p(a, a), p(b, b)]",
            "ar3(f(a, b), R)" to "[p(1, a), p(2, b)]",
            "sa1(1, 2, 3, R)" to "[r(1, 2, 3)]",
            "sa2(1, 2, R)" to "[t(1, 2, r(2, 1, 1))]",
            "sa3(1, 2, R)" to "[t(1, 2, 2, r(2, 2, 2))]",
        )) {
            assertEquals(expected, all("R", goal), goal)
        }
    }

    @Test
    fun aHeadVariableTheFirstGoalTakesTheRegisterOf() {
        assertEquals("[r(2, t)]", all("R", "sl(1, [2 | t], R)"))
        assertEquals("[]", all("R", "sl(1, [3 | t], R)"))
        assertEquals("[2]", all("Y", "(sl(1, [Y | t], _))"))
    }

    @Test
    fun anExpressionLeftToTheBuiltinIsReadBeforeItsTarget() {
        assertEquals("[r(2.5, 1.5)]", all("R", "fl(1.5, R)"))
        assertEquals("[r(18446744073709551616, 4294967296)]", all("R", "fo(4294967296, R)"))
        assertEquals("[6.5]", all("R", "fg(3, 0.5, R)"))
    }

    @Test
    fun theOrderOfIntegersBeyondTheWords() {
        val huge = "18446744073709551616"
        assertTrue(holds("$huge @> 1"))
        assertFalse(holds("$huge @< 1"))
        assertTrue(holds("1 @< $huge"))
        assertFalse(holds("$huge @< $huge"))
        assertTrue(holds("$huge @> 9223372036854775807"))
        assertTrue(holds("-$huge @< 0"))
    }

    @Test
    fun manyChoicePointsSolveAfterSolve() {
        // (the arrays of a machine come back from the one before, those beside them must follow)
        assertEquals("[yes]", all("Y", "(manycps(3000), Y = yes, !)").ifEmpty { "[yes]" })
        assertTrue(holds("manycps(3000)"))
        assertTrue(holds("manycps(3000)"))
    }

    @Test
    fun termsBuiltWholeOrUnified() {
        for ((goal, expected) in listOf(
            "tp1(f(g(1), h(2, 1)), X, Y), R = t(X, Y)" to "t(1, 2)",
            "tp1(f(g(1), h(2, 3)), _, _), R = no" to null,
            "tp1(S, 1, 2), S = f(g(A), h(B, C)), R = t(A, B, C)" to "t(1, 2, 1)",
            "tp1(S, X, Y), X = 1, Y = 2, S = f(g(A), h(B, C)), R = t(A, B, C)" to "t(1, 2, 1)",
            "tp1(f(S, h(2, 1)), _, _), S = g(R)" to "1",
            "tp2(1, S, 9), S = f(g(A), [B, C | D], h(k(E))), C = 7, R = t(A, B, C, D, E)" to "t(1, 1, 7, 9, 7)",
            "tp2(1, f(g(1), [1, 2 | 3], h(k(2))), R)" to "3",
            "tp2(1, f(g(2), [1, 2 | 3], h(k(2))), R)" to null,
            "tp3(S, 1, 2), S = [_, _ | R]" to "2",
            "tp3(L, 1, []), L = [f(A), g(B, _)], R = t(A, B)" to "t(1, 1)",
            "tp3([f(1), g(1, 5), z], X, T), R = t(X, T)" to "t(1, [z])",
            "tp3([f(1), g(2, 5)], _, _), R = no" to null,
            "tp4(S, 1), S = f(g(h(i(R))))" to "1",
            "tp4(f(g(h(i(1)))), R)" to "1",
            "tp4(f(g(h(1))), R)" to null,
            "tp5(a, S, 1), S = f(g(A), B), R = t(A, B)" to "t(1, 1)",
            "tp6(S, R), S = f(g(1), _)" to "1",
            "tp6(f(g(1), k(1, b)), R)" to "1",
            "tp6(f(g(1), k(2, b)), R)" to null,
            "tp7(S), S = f(1, g(2), 3), R = yes" to "yes",
            "tp7(f(1, 2, 3)), R = yes" to null,
            "tp8(S, 1, R), S = f(g(_, 2), _)" to "t(1, 2)",
        )) {
            val answer = all("R", "($goal)")
            assertEquals(if (expected == null) "[]" else "[$expected]", answer, goal)
        }
    }

    @Test
    fun clausesThatCommitWithNoChoicePoint() {
        for ((goal, expected) in listOf(
            "cm1(f(1, 2), 3, R)" to "[g(1, h(2, 3))]",
            "cm1(f(1, 2), 3, g(1, R))" to "[h(2, 3)]",
            "cm1(f(1, 2), 3, g(9, R))" to "[]",
            "cm1(z, 3, R)" to "[other]",
            "(cm2(f(1), B, S), R = t(B, S))" to "[t(done, g(1))]",
            "(cm2(f(1), B, g(2)), R = B)" to "[]",
            "(cm2(z, B, S), R = t(B, S))" to "[t(second, none)]",
            "cm3(f(1), R)" to "[1]",
            "cm3(z, R)" to "[other]",
            "(cm3(f(1), 2), R = no)" to "[]",
            "cm4(f(1), R)" to "[g(1)]",
            "cm4(z, R)" to "[last]",
            "(cm5(a, 1), R = yes)" to "[yes]",
            "cm5(z, R)" to "[b]",
            "cm6(f(1, 1), R)" to "[same]",
            "cm6(f(1, 2), R)" to "[other]",
        )) {
            assertEquals(expected, all("R", goal), goal)
        }
        assertTrue(holds("cm4(f(1), g(1))"))
        assertTrue(holds("cm4(f(1), g(z))"))
        assertFalse(holds("cm4(f(1), g(2)), fail"))
    }

    @Test
    fun argumentsPutAsAParallelMove() {
        for ((goal, expected) in listOf(
            "rot(1, 2, 3, R)" to "[r(2, 3, 1)]",
            "rot2(1, 2, 3, R)" to "[r(3, 1, 2)]",
            "swp(1, 2, 3, R)" to "[r(2, 1, 3)]",
            "two(1, 2, R)" to "[r(2, 2, 1)]",
            "mix(1, 2, R)" to "[r(f(1), 2, 2)]",
            "gua(1, 2, 3, R)" to "[r(3, 1, 2)]",
            "gua(0, 2, 3, R)" to "[]",
            "gua(1.5, 2, 3, R)" to "[r(3, 1.5, 2)]",
            "typ2(a, b, R)" to "[r(b, a, c)]",
            "hed(f(1), 2, 3, R)" to "[r(3, 2, 1)]",
            "nws(1, 2, R)" to "[r(2, 1, 2)]",
            "lst(1, 2, 3, R)" to "[l(3, 1, 2)]",
        )) {
            assertEquals(expected, all("R", goal), goal)
        }
    }

    @Test
    fun clausesThatOnlyTest() {
        val big = "3000000000"
        for ((goal, expected) in listOf(
            "lg(1, 2, R)" to "[lt]",
            "lg(2, 1, R)" to "[gt]",
            "lg(1, 1, R)" to "[eq]",
            "lg(1.5, 2, R)" to "[lt]",
            "lg(2, 1.5, R)" to "[gt]",
            "lg(1.5, 1.5, R)" to "[eq]",
            "lg($big, 1, R)" to "[gt]",
            "lg(1, $big, R)" to "[lt]",
            "lg($big, $big, R)" to "[eq]",
            "catch(lg(_, 1, _), error(_, _), R = e)" to "[e]",
            "catch(lg(1, a, _), error(_, _), R = e)" to "[e]",
            "tg(1, R)" to "[int]",
            "tg(a, R)" to "[isa]",
            "tg(_, R)" to "[free]",
            "tg(b, R)" to "[other]",
            "gm(1, 2, 3, 3, R)" to "[yes]",
            "gm(1, 2, 3, 4, R)" to "[]",
            "gm(2, 1, 3, 3, R)" to "[no]",
            "gm(2, 1, 3, 4, R)" to "[]",
            "gm(2.5, 1, 3, 3, R)" to "[no]",
        )) {
            assertEquals(expected, all("R", goal), goal)
        }
    }

    @Test
    fun theKeysWithinTheFirstArgument() {
        for ((goal, expected) in listOf(
            "in1(f(a, _), R)" to "[one, three]",
            "in1(f(a, d), R)" to "[one]",
            "in1(f(b, c), R)" to "[two, three]",
            "in1(f(z, c), R)" to "[three]",
            "in1(f(z, z), R)" to "[]",
            "in1(f(_, _), R)" to "[one, two, three]",
            "in1(g(a), R)" to "[four]",
            "in1(g(b), R)" to "[]",
            "in1(_, R)" to "[one, two, three, four]",
            "in2([a], R)" to "[one]",
            "in2([b, a], R)" to "[two]",
            "in2([c], R)" to "[]",
            "in2([], R)" to "[three]",
            "in2([_], R)" to "[one, two]",
            "in2(_, R)" to "[one, two, three]",
        )) {
            assertEquals(expected, all("R", goal), goal)
        }
    }

    @Test
    fun conditionsWithNoChoicePoint() {
        assertEquals("[big]", all("R", "co1(2, R)"))
        assertEquals("[small]", all("R", "co1(1, R)"))
        assertEquals("[none]", all("R", "co1(0, R)"))
        assertEquals("[isa, other]", all("R", "co2(a, R)"))
        assertEquals("[isb, other]", all("R", "co2(b, R)"))
        assertEquals("[other]", all("R", "co2(c, R)"))
        assertEquals("[big]", all("R", "co3(2, R)"))
        assertEquals("[other]", all("R", "co3(0, R)"))
        assertEquals("[2]", all("R", "co4(f(2), R)"))
        assertEquals("[low(1)]", all("R", "co4(f(1), R)"))
        assertEquals("[no]", all("R", "co4(g, R)"))
        assertEquals("[notbig, alternative]", all("R", "co5(0, R)"))
        assertEquals("[alternative]", all("R", "co5(2, R)"))
        assertEquals("[yes]", all("Y", "(co6(_), Y = yes)"))
        assertEquals("[]", all("Y", "(co6(b), Y = yes)"))
        assertEquals("[2]", all("R", "co7(R)"))
        assertEquals("[first, third]", all("R", "co8(2, R)"))
        assertEquals("[second, third]", all("R", "co8(0, R)"))
        assertEquals("[a]", all("R", "co9(3, R)"))
        assertEquals("[b]", all("R", "co9(2, R)"))
        assertEquals("[c]", all("R", "co9(1, R)"))
        assertEquals("[d]", all("R", "co9(0, R)"))
        assertEquals("[low, 3]", all("R", "co10(_, R)"))
        assertEquals("[low]", all("R", "co11(1, R)"))
        assertEquals("[high]", all("R", "co11(2, R)"))
        assertEquals("[low]", all("R", "co11(1.25, R)"))
        assertEquals("[e]", all("R", "co12(_, R)"))
        assertEquals("[big]", all("R", "co12(2, R)"))
    }

    @Test
    fun choicePointsDelayed() {
        assertEquals("[a, b, c]", all("R", "dl1(f(1), 1, R)"))
        assertEquals("[c]", all("R", "dl1(f(1), 2, R)"))
        assertEquals("[c]", all("R", "dl1(g(1), 1, R)"))
        assertEquals("[big]", all("R", "dl2(2, R)"))
        assertEquals("[small]", all("R", "dl2(1, R)"))
        assertEquals("[none]", all("R", "dl2(0, R)"))
        assertEquals("[instantiation]", all("R", "catch(dl2(_, _), error(instantiation_error, _), R = instantiation)"))
        assertEquals("[2]", all("R", "dl3(2, R)"))
        assertEquals("[neg]", all("R", "dl3(-1, R)"))
        assertEquals("[]", all("R", "dl3(0, R)"))
        assertEquals("[yes]", all("Y", "(dl4(_, 0), Y = yes)"))
        assertEquals("[bound]", all("V", "(dl4(V, 2), nonvar(V))"))
        assertEquals("[1, 2, 3]", all("X", "dl6(X)"))
        assertEquals("[2]", all("X", "dl7(X)"))
        assertEquals("[one, two]", all("R", "dl8([1, 2], _, _, R)"))
        assertEquals("[two]", all("R", "dl8([1, 2], 1, _, R)"))
        assertEquals("[small, big, big]", all("R", "dl9(_, R)"))
        assertEquals("[f(2), g(2)]", all("X", "dl10(X, 2)"))
        assertEquals("[g(1)]", all("X", "dl10(X, 1)"))
        assertEquals("[big, alternative]", all("R", "dl11(2, R)"))
        assertEquals("[other, alternative]", all("R", "dl11(0, R)"))
        assertEquals("[e]", all("R", "catch((dl6(X), dl2(a, X), R = n), error(_, _), R = e)"))
    }

    @Test
    fun argWithAVariableIndex() {
        assertEquals("[b]", all("R", "av1(2, f(a, b), R)"))
        assertEquals("[]", all("R", "av1(3, f(a, b), R)"))
        assertEquals("[]", all("R", "av1(0, f(a, b), R)"))
        assertEquals("[a]", all("R", "av1(1, [a, b], R)"))
        assertEquals("[[b]]", all("R", "av1(2, [a, b], R)"))
        assertEquals("[a, b]", all("R", "av1(_, f(a, b), R)"))
        assertEquals("[r(b, 2)]", all("R", "av2(2, f(a, b), R)"))
        assertEquals("[r(a, 1), r(b, 2)]", all("R", "av2(_, f(a, b), R)"))
        assertEquals("[r(1, a), r(2, b)]", all("R", "av3(f(a, b), R)"))
        assertEquals("[1]", all("I", "av4(I, f(a, b), _)"))
        assertEquals("[]", all("I", "av4(2, f(a, b), _)"))
        assertEquals("[b]", all("R", "av5(2, f(a, b), R)"))
        assertEquals("[1]", all("Y", "av6(1, g(f(1)), f(Y))"))
    }

    @Test
    fun functorBuildsATerm() {
        assertEquals("[g]", all("N", "(fb(g, 2, R), functor(R, N, _))"))
        assertEquals("[2]", all("A", "(fb(g, 2, R), functor(R, _, A))"))
        assertEquals("[2]", all("A", "(fb('.', 2, R), functor(R, _, A))"))
        assertEquals("[yes]", all("Y", "(fb('.', 2, [_ | _]), Y = yes)"))
        assertEquals("[a]", all("R", "fb(a, 0, R)"))
        assertEquals("[1]", all("R", "fb(1, 0, R)"))
        assertEquals("[2.5]", all("R", "fb(2.5, 0, R)"))
        assertEquals("[yes]", all("Y", "(fbn(g, g(_, _)), Y = yes)"))
    }

    @Test
    fun aCollectionFallsNotUnderAnInlineBuiltinLeftToTheBuiltin() {
        // (each kept/2 allocates and then has the builtin called, the temporary W living across it: past the threshold, there)
        assertEquals("[yes]", all("Y", "(churn(12000), Y = yes)"))
    }

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
    fun callOfAVariableIsCallOfTheTerm() {
        assertEquals("[a, a]", all("R", "viaCall(member(_, [1, 2]), R)"))
        assertEquals("[1, 2]", all("X", "viaCall(member(X, [1, 2]), _)"))
        assertEquals("[1, 2]", all("X", "viaLast(member(X, [1, 2]))"))
        assertEquals("[1, 2]", all("X", "viaOnly(member(X, [1, 2]))"))
        assertEquals("[1, 2]", all("X", "viaTemporary(member(X, [1, 2]))"))
        assertEquals("[1, 2]", all("R", "cutStaysIn(R)"))
        assertEquals("[1]", all("R", "cutAfter(R)"))
        assertEquals("[1]", all("X", "viaLast((member(X, [1, 2]), !))"))
        assertTrue(holds("notCalled(fail)"))
        assertFalse(holds("notCalled(true)"))
        for (goal in listOf("viaCall(1, _)", "viaLast(1)", "viaOnly(1)", "viaLast((true, 1))")) {
            assertEquals(outcome("call(1)"), outcome(goal), goal)
        }
        for (goal in listOf("viaCall(_, _)", "viaLast(_)", "viaOnly(_)")) {
            assertEquals(outcome("call(_)"), outcome(goal), goal)
        }
    }

    @Test
    fun theMergedInstructionsDoWhatTheirPartsDid() {
        assertTrue(holds("same0(0)"))
        assertTrue(holds("same0(1)"))
        assertEquals(1, solver.solve(Struct.parse("same0(0)", solver.operators)).count { it.isYes })
        assertEquals("k(l(1, 3), m(2, 4))", answer("nested(f(g(1, 2), h(3, 4)), R)"))
        assertEquals("f(g(1, 2), h(3, 4))", answer("nested(R, k(l(1, 3), m(2, 4)))"))
        assertFalse(holds("nested(f(g(1, 2), h(3, 4)), k(l(1, 3), m(2, 5)))"))
        assertFalse(holds("nested(f(g(1, 2), x), R)"))
        assertEquals("k(l(1, 3), m(2, 4))", answer("builds(1, 2, 3, 4, R)"))
        assertEquals("1", answer("cutThenCall(1, Y, R)"))
        assertTrue(holds("threeSlots(1, 2, 3)"))
        assertFalse(holds("threeSlots(1, 2, 4)"))
        assertFalse(holds("threeSlots(1, 5, 3)"))
        assertTrue(holds("twoSlots(1, 2)"))
        assertFalse(holds("twoSlots(1, 3)"))
        assertTrue(holds("twoLast(1, 2)"))
        assertFalse(holds("twoLast(2, 2)"))
    }

    @Test
    fun aGuardMovingAVariableLeavesTheArgumentsToTheNextClause() {
        assertEquals("a", answer("moved(f(a), R)"))
        assertEquals("a", answer("moved(a, R)"))
        assertEquals(1, solver.solve(Struct.parse("moved(a, R)", solver.operators)).count { it.isYes })
        assertFalse(holds("moved(a, b)"))
    }

    @Test
    fun alternativesNotLookedAtAheadAreStillRight() {
        assertEquals("1", answer("same(a, a, R)"))
        assertEquals("0", answer("same(a, b, R)"))
        assertEquals(1, solver.solve(Struct.parse("same(a, a, R)", solver.operators)).count { it.isYes })
        assertTrue(holds("same(a, a, 0)"))
        assertFalse(holds("same(a, b, 1)"))
        assertEquals("8", answer("same8(x, x, R)"))
        assertEquals("9", answer("same8(x, y, R)"))
        assertEquals(2, solver.solve(Struct.parse("same8(a, a, R)", solver.operators)).count { it.isYes })
        assertEquals("g(1, 2)", answer("commit(f(1), 2, R)"))
        assertEquals(1, solver.solve(Struct.parse("commit(f(1), 2, R)", solver.operators)).count { it.isYes })
        assertTrue(holds("commit(f(1), 2, none)"))
        assertFalse(holds("commit(f(1), 2, other)"))
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
