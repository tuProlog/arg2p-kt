package it.unibo.tuprolog.argumentation.core.engine.vm

import it.unibo.tuprolog.argumentation.core.engine.ClauseSource
import it.unibo.tuprolog.argumentation.core.engine.DelegatedCall
import it.unibo.tuprolog.argumentation.core.engine.Delegation
import it.unibo.tuprolog.argumentation.core.engine.ExecutionEnvironment
import it.unibo.tuprolog.argumentation.core.engine.Inlining
import it.unibo.tuprolog.argumentation.core.engine.Sourcing
import it.unibo.tuprolog.argumentation.core.engine.stdlib.DefaultBuiltins
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.library.Library
import it.unibo.tuprolog.solve.library.Runtime
import it.unibo.tuprolog.solve.primitive.Solve
import it.unibo.tuprolog.theory.Theory
import it.unibo.tuprolog.theory.parsing.parse
import kotlin.system.measureNanoTime
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertTrue

/**
 * The machine spike: correctness on small queries (control, the primitives of the standard library through the
 * facade, garbage collection), then the inference rate on the benchmarks of lips.py
 */
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
        coll(N, L) :- range(1, N, R), findall(X-Y, (member(X, R), Y is X * 2), L).
        p(1). p(2). p(3).
        q(X, Y) :- p(X), p(Y), X < Y.
        first(X) :- p(X), !.
        sign(X, S) :- (X < 0 -> S = neg ; X =:= 0 -> S = zero ; S = pos).
        either(X) :- (X = a ; X = b).
        notp(X) :- \+ p(X).
        many(1, a). many(2, b). many(3, c). many(4, d). many(5, e). many(6, f). many(7, g). many(8, h).
        many(9, i). many(X, var) :- integer(X), X > 100. many(10, j). many(11, k). many(12, l).
        safe(X, R) :- catch(X, error(E, _), R = caught(E)).
        '->'(k(s1), b).
        '->'(k(s2), c).
        strict(Id) :- '->'(k(Id), _).
        prim(0) :- !.
        prim(N) :-
            atom_length(hello, 5), functor(T, f, 3), arg(1, T, a), T =.. [f | Args], length(Args, 3),
            atom_concat(ab, cd, abcd), M is N - 1, prim(M).
        """.trimIndent()

    private fun machine(gcMinimum: Int = Machine.GC_MIN): Machine {
        val program = Program()
        program.add(Theory.parse(theory))
        val machine = Machine(program, gcMinimum)
        machine.prepare()
        machine.facade(ExecutionEnvironment(libraries = Runtime.of(DefaultBuiltins, delegations), startTime = 0))
        return machine
    }

    // A library of a "module" (secret/1 lives there alone) and of two delegated calls: in_module(G) runs G with the
    // module's libraries, looked_up(G) resolves G with clauses read when called
    private val moduleLibraries =
        Runtime.of(Library.of(alias = "test.module", clauses = Theory.parse("secret(42). secret(43).").clauses.toList()))

    private val inModule =
        object : DelegatedCall {
            override fun delegate(request: Solve.Request<ExecutionContext>): Delegation =
                Inlining(request.arguments[0] as Struct, moduleLibraries)

            override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> = throw UnsupportedOperationException()
        }

    private val lookedUp =
        object : DelegatedCall {
            override fun delegate(request: Solve.Request<ExecutionContext>): Delegation =
                Sourcing(
                    request.arguments[0] as Struct,
                    ClauseSource { _, _ -> Theory.parse("stored(1). stored(2). stored(X) :- X = 3.").clauses.toList() },
                )

            override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> = throw UnsupportedOperationException()
        }

    private val delegations =
        Library.of(
            alias = "test.delegations",
            primitives = mapOf(Signature("in_module", 1) to inModule, Signature("looked_up", 1) to lookedUp),
        )

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

    @Test
    fun control() {
        val m = machine()
        assertEquals(listOf("S = neg"), answers(m, "sign(-3, S)"))
        assertEquals(listOf("S = zero"), answers(m, "sign(0, S)"))
        assertEquals(listOf("S = pos"), answers(m, "sign(7, S)"))
        assertEquals(listOf("X = a", "X = b"), answers(m, "either(X)"))
        assertEquals(listOf(""), answers(m, "either(b)"))
        assertEquals(listOf(""), answers(m, "notp(4)"))
        assertEquals(emptyList(), answers(m, "notp(2)"))
        assertEquals(listOf("X = 1"), answers(m, "(p(X), X > 0 -> true ; fail)"))
        assertEquals(listOf("L = [a, b]"), answers(m, "findall(X, either(X), L)"))
    }

    @Test
    fun primitivesThroughTheFacade() {
        val m = machine()
        assertEquals(listOf("N = 5"), answers(m, "atom_length(hello, N)"))
        assertTrue(answers(m, "functor(T, f, 2)").single().matches(Regex("T = f\\(_\\w*, _\\w*\\)")))
        assertEquals(listOf("L = [f, a, b]"), answers(m, "f(a, b) =.. L"))
        assertEquals(listOf("X = 1", "X = 2", "X = 3"), answers(m, "between(1, 3, X)"))
        assertEquals(listOf("L = [1, 2, 3]"), answers(m, "findall(X, between(1, 3, X), L)"))
        assertEquals(listOf("X = 2"), answers(m, "between(1, 3, X), X > 1, !"))
        assertEquals(listOf("A = abcd"), answers(m, "atom_concat(ab, cd, A)"))
        assertEquals(listOf("L = [1, 2]"), answers(m, "assertz(dyn(1)), assertz(dyn(2)), findall(X, dyn(X), L)"))
        assertEquals(listOf("L = [2]"), answers(m, "retract(dyn(1)), findall(X, dyn(X), L)"))
        assertEquals(listOf("L = [3]"), answers(m, "findall(X, (member(X, [1, 2, 3]), \\+ dyn(X), X > 2), L)"))
    }

    @Test
    fun exceptions() {
        val m = machine()
        assertEquals(listOf("R = caught(evaluation_error(zero_divisor))"), answers(m, "safe(X is 1 // 0, R)"))
        assertEquals(listOf("R = caught(type_error(evaluable, foo/0))"), answers(m, "safe(X is foo + 1, R)"))
        assertEquals(listOf("X = recovered"), answers(m, "catch(throw(oops), oops, X = recovered)"))
        assertEquals(listOf("E = oops"), answers(m, "catch(catch(throw(oops), other, true), E, true)"))
        assertEquals(listOf("X = 1", "X = 2"), answers(m, "catch(member(X, [1, 2]), _, true)"))
        assertEquals(listOf("R = caught(type_error(integer, a))"), answers(m, "safe(atom_length(abc, a), R)"))
        val stopping = "findall(X, catch((member(X, [a, b, c]), (X == c -> throw(stop) ; true)), stop, fail), L)"
        assertEquals(listOf("L = [a, b]"), answers(m, stopping))
        try {
            answers(m, "throw(unhandled)")
            kotlin.test.fail("no halt")
        } catch (halt: Machine.Halt) {
            assertTrue(halt.exception.message!!.contains("unhandled"), halt.exception.message)
        }
    }

    @Test
    fun delegatedCalls() {
        val m = machine()
        assertEquals(emptyList(), answers(m, "secret(X)"))
        assertEquals(listOf("X = 42", "X = 43"), answers(m, "in_module(secret(X))"))
        assertEquals(listOf("L = [42, 43]"), answers(m, "findall(X, in_module(secret(X)), L)"))
        assertEquals(listOf("X = 42"), answers(m, "in_module((secret(X), !))"))
        assertEquals(listOf("X = 1", "X = 2", "X = 3"), answers(m, "looked_up(stored(X))"))
        assertEquals(listOf("X = 2"), answers(m, "looked_up(stored(X)), X > 1, !"))
        assertEquals(listOf("L = [42, 42, 42, 43, 43, 43]"), answers(m, "findall(X, (in_module(secret(X)), p(_)), L)"))
    }

    @Test
    fun theoryClausesOfBuiltins() {
        val m = machine()
        // ->/2 is if-then as a goal, and the strict rules of a theory are its clauses too (the engine concatenates)
        assertEquals(listOf("X = 1"), answers(m, "(true -> X = 1)"))
        assertEquals(listOf("L = [s1, s2]"), answers(m, "findall(Id, '->'(k(Id), _), L)"))
        assertEquals(listOf("Id = s1", "Id = s2"), answers(m, "strict(Id)"))
        assertEquals(listOf("X = 2"), answers(m, "(fail -> X = 1 ; X = 2)"))
    }

    @Test
    fun indexing() {
        val m = machine()
        assertEquals(listOf("Y = g"), answers(m, "many(7, Y)"))
        assertEquals(listOf("Y = var"), answers(m, "many(200, Y)"))
        assertEquals(emptyList(), answers(m, "many(50, Y)"))
        assertEquals(12, answers(m, "many(X, Y)").size)
        assertEquals(listOf("X = 10"), answers(m, "many(X, j)"))
    }

    @Test
    fun garbageCollection() {
        val m = machine()
        // A deterministic run far beyond the collection threshold: bounded by the collector
        assertEquals(listOf(""), answers(m, "bench(20000)"))
        assertTrue(m.collections > 0, "no collection ran")
        assertTrue(m.heap.size < (1 shl 22), "the heap grew to ${m.heap.size} cells")
        // Collections during a findall, with choice points and the trail alive
        assertEquals(listOf(""), answers(m, "\\+ \\+ (coll(10000, L), length(L, 10000))"))
        // A long list surviving collections
        assertEquals(listOf(""), answers(m, "\\+ \\+ (range(1, 5000, L), bench(2000), length(L, 5000))"))
    }

    private fun best(
        m: Machine,
        goal: String,
        runs: Int = 5,
    ): Double = (1..runs).minOf { measureNanoTime { m.solve(Struct.parse(goal)).first() } } / 1e6

    @Test
    fun inferenceRate() {
        for (shift in listOf(22)) {
            println("-- collection threshold ${1 shl shift} cells")
            inferenceRate(machine(1 shl shift))
        }
    }

    private fun inferenceRate(m: Machine) {
        repeat(3) {
            m.solve(Struct.parse("bench(200)")).first()
            m.solve(Struct.parse("loop(20000)")).first()
            m.solve(Struct.parse("coll(2000, _)")).first()
            m.solve(Struct.parse("prim(2000)")).first()
        }
        for (n in listOf(500, 2000, 8000)) {
            val tn = best(m, "bench($n)")
            println("nrev30 x$n: ${"%.0f".format(tn)} ms  ->  ${"%.2f".format(n * 528 / tn / 1e3)} M LIPS (${m.collections} collections)")
        }
        val n1 = 20000
        val t1 = best(m, "bench($n1)")
        println("nrev30 x$n1: ${"%.0f".format(t1)} ms  ->  ${"%.2f".format(n1 * 528 / t1 / 1e3)} M LIPS (${m.collections} collections)")
        val n2 = 2000000
        val t2 = best(m, "loop($n2)")
        println("loop($n2): ${"%.0f".format(t2)} ms  ->  ${"%.2f".format(n2 / t2 / 1e3)} M iterations/s")
        val n3 = 100000
        val t3 = best(m, "coll($n3, _)")
        println("findall/member x$n3: ${"%.0f".format(t3)} ms  ->  ${"%.0f".format(n3 / t3)} K items/s")
        val n4 = 20000
        val t4 = best(m, "prim($n4)")
        println("primitives x$n4 (6 calls each): ${"%.0f".format(t4)} ms  ->  ${"%.0f".format(n4 * 6 / t4)} K primitive calls/s")
    }
}
