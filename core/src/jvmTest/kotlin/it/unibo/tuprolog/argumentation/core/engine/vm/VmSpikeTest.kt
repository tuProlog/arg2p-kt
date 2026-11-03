package it.unibo.tuprolog.argumentation.core.engine.vm

import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.theory.Theory
import it.unibo.tuprolog.theory.parsing.parse
import kotlin.system.measureNanoTime
import kotlin.test.Test
import kotlin.test.assertEquals

/** The machine spike: correctness on small queries, then the inference rate on the benchmarks of lips.py */
class VmSpikeTest {
    private val theory =
        """
        app([], L, L).
        app([H|T], L, [H|R]) :- app(T, L, R).
        nrev([], []).
        nrev([H|T], R) :- nrev(T, RT), app(RT, [H], R).
        range(N, N, [N]) :- !.
        range(I, N, [I|T]) :- I1 is I + 1, range(I1, N, T).
        bench(0) :- !.
        bench(N) :- range(1, 30, L), nrev(L, _), M is N - 1, bench(M).
        loop(0) :- !.
        loop(N) :- M is N - 1, loop(M).
        member(X, [X|_]).
        member(X, [_|T]) :- member(X, T).
        coll(N, L) :- range(1, N, R), findall(X-Y, (member(X, R), Y is X * 2), L).
        p(1). p(2). p(3).
        q(X, Y) :- p(X), p(Y), X < Y.
        first(X) :- p(X), !.
        """.trimIndent()

    private fun machine(): Machine {
        val program = Program()
        program.add(Theory.parse(theory))
        return Machine(program).also { it.prepare() }
    }

    private fun answers(
        m: Machine,
        goal: String,
    ): List<String> =
        m
            .solve(Struct.parse(goal))
            .map { bindings ->
                bindings.entries.sortedBy { it.key.name }.joinToString(", ") { (v, t) -> "${v.name} = $t" }
            }.toList()

    @Test
    fun smallQueries() {
        val m = machine()
        assertEquals(listOf("L = [1, 2, 3]"), answers(m, "app([1, 2], [3], L)"))
        assertEquals(listOf("R = [3, 2, 1]"), answers(m, "nrev([1, 2, 3], R)"))
        assertEquals(listOf("X = 1, Y = 2", "X = 1, Y = 3", "X = 2, Y = 3"), answers(m, "q(X, Y)"))
        assertEquals(listOf("X = 1"), answers(m, "first(X)"))
        assertEquals(listOf("L = [1, 2, 3, 4]"), answers(m, "range(1, 4, L)"))
        assertEquals(listOf("L = ['-'(1, 2), '-'(2, 4), '-'(3, 6)]"), answers(m, "coll(3, L)"))
        assertEquals(listOf("X = [], Y = [1, 2]", "X = [1], Y = [2]", "X = [1, 2], Y = []"), answers(m, "app(X, Y, [1, 2])"))
        assertEquals(emptyList(), answers(m, "app([1], [2], [3])"))
        assertEquals(listOf("L = ['-'([], [1, 2]), '-'([1], [2]), '-'([1, 2], [])]"), answers(m, "findall(X-Y, app(X, Y, [1, 2]), L)"))
    }

    private fun best(
        m: Machine,
        goal: String,
        runs: Int = 5,
    ): Double = (1..runs).minOf { measureNanoTime { m.solve(Struct.parse(goal)).first() } } / 1e6

    @Test
    fun inferenceRate() {
        val m = machine()
        repeat(3) {
            m.solve(Struct.parse("bench(200)")).first()
            m.solve(Struct.parse("loop(20000)")).first()
            m.solve(Struct.parse("coll(2000, _)")).first()
        }
        val n1 = 2000
        val t1 = best(m, "bench($n1)")
        println("nrev30 x$n1: ${"%.0f".format(t1)} ms  ->  ${"%.2f".format(n1 * 528 / t1 / 1e3)} M LIPS")
        val n2 = 200000
        val t2 = best(m, "loop($n2)")
        println("loop($n2): ${"%.0f".format(t2)} ms  ->  ${"%.2f".format(n2 / t2 / 1e3)} M iterations/s")
        val n3 = 10000
        val t3 = best(m, "coll($n3, _)")
        println("findall/member x$n3: ${"%.0f".format(t3)} ms  ->  ${"%.0f".format(n3 / t3)} K items/s")
    }
}
