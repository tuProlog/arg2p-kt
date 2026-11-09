package it.unibo.tuprolog.argumentation.engine.vm

import it.unibo.tuprolog.argumentation.engine.stdlib.DefaultBuiltins
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.library.Library
import it.unibo.tuprolog.solve.library.Runtime
import it.unibo.tuprolog.solve.primitive.Primitive
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertNotNull

/** The natives of the standard library answer as the primitives they stand in for: both run on the same goals */
class StdlibNativesTest {
    // The standard library with its primitives behind other objects: the natives do not stand in for them
    private val plain =
        Library.of(
            alias = "test.plain",
            primitives = DefaultBuiltins.primitives.mapValues { (_, primitive) -> Primitive { request -> primitive.solve(request) } },
            clauses = DefaultBuiltins.clauses,
            functions = DefaultBuiltins.functions,
        )

    private fun machine(library: Library): Machine =
        Machine(Program(), ExecutionEnvironment(libraries = Runtime.of(library), startTime = 0))

    private val native = machine(DefaultBuiltins)
    private val facade = machine(plain)

    private fun answers(
        machine: Machine,
        goal: String,
    ): String =
        try {
            machine
                .solve(Struct.parse(goal))
                .take(5)
                .toList()
                .joinToString(" ; ") { bindings ->
                    bindings.entries.sortedBy { it.key.name }.joinToString(", ") { (v, t) -> "${v.name} = $t" }
                }.ifEmpty { "no" }
                .replace(Regex("""(?<![A-Za-z0-9_'])(_\w*|[A-Z]\w*_\d+)"""), "_")
        } catch (halt: Machine.Halt) {
            "error: ${halt.exception.message}"
        }

    private fun same(goals: List<String>) {
        val different =
            goals.mapNotNull { goal ->
                val a = answers(native, goal)
                val b = answers(facade, goal)
                if (a == b) null else "$goal\n  native: $a\n  facade: $b"
            }
        assertEquals("", different.joinToString("\n"), "${different.size} of ${goals.size} goals differ")
    }

    private val terms =
        listOf(
            "X",
            "1",
            "-3",
            "0",
            "2.5",
            "1.0",
            "-0.5",
            "100000000000",
            "-100000000000",
            "a",
            "b",
            "'B'",
            "'é'",
            "'_x'",
            "'hello world'",
            "'a b'",
            "[]",
            "'[]'",
            "{}",
            "true",
            "fail",
            "[a]",
            "[b]",
            "[a, b]",
            "[a, b, c]",
            "[a | T]",
            "[X, X]",
            "f(a)",
            "f(b)",
            "g(a)",
            "f(a, b)",
            "f(b, a)",
            "f(X)",
            "f(X, X)",
            "f(X, Y)",
            "g(f(a), [1, 2])",
            "(a, b)",
            "(a :- b)",
            "'.'(a, b)",
            "- a",
            "1 + 2",
        )

    @Test
    fun theTypeTests() =
        same(
            terms.flatMap { t ->
                listOf("atom", "atomic", "callable", "compound", "float", "integer", "number", "ground").map { "$it($t)" }
            },
        )

    @Test
    fun theComparisons() =
        same(
            terms.flatMap { a ->
                terms.flatMap { b -> listOf("==", "\\==", "\\=", "@<", "@=<", "@>", "@>=").map { "($a) $it ($b)" } }
            },
        )

    @Test
    fun aVariableComparedWithItself() =
        same(
            listOf(
                "X == X",
                "X \\== X",
                "X == Y",
                "f(X, Y) == f(X, Y)",
                "f(X, Y) == f(Y, X)",
                "X = Y, X == Y",
                "X = a, X == a",
                "X @< X",
                "X @=< X",
                "X @>= X",
                "f(X) @=< f(X)",
                "\\+ X \\= a, var(X)",
                "X \\= a",
                "f(X, b) \\= f(a, X)",
                "copy_term(X, Y), X == Y",
                "copy_term(f(X, X, Y), Z)",
                "copy_term(a, a)",
                "copy_term(a, b)",
                "copy_term(X, a), var(X)",
                "copy_term([X, Y | T], L)",
            ),
        )

    @Test
    fun functorAndArg() =
        same(
            terms.flatMap { t ->
                listOf("functor($t, N, A)", "functor($t, f, 2)", "functor($t, N, 0)", "functor($t, N, a)", "functor($t, N, -1)")
            } +
                terms.flatMap { t ->
                    listOf(
                        "arg(1, $t, A)",
                        "arg(2, $t, A)",
                        "arg(0, $t, A)",
                        "arg(3, $t, A)",
                        "arg(-1, $t, A)",
                        "arg(N, $t, A)",
                        "arg(a, $t, A)",
                    )
                } +
                listOf(
                    "functor(T, f, 2), T = f(a, b)",
                    "functor(T, f, 0)",
                    "functor(T, a, 1)",
                    "functor(T, 1, 0)",
                    "functor(T, 1, 1)",
                    "functor(T, f(a), 1)",
                    "functor(T, '.', 2)",
                    "functor(T, N, 2)",
                    "functor(T, f, A)",
                    "functor(f(a), f, 1)",
                    "functor(f(a), g, 1)",
                    "functor(f(a), f, 2)",
                    "arg(1, f(a), a)",
                    "arg(1, f(a), b)",
                    "arg(1, f(X), a), X == a",
                    "functor(1, 1, 0)",
                    "functor(1.5, N, A)",
                ),
        )

    @Test
    fun univ() =
        same(
            terms.flatMap { t -> listOf("$t =.. L", "$t =.. [H | T0]", "$t =.. []", "$t =.. a", "$t =.. [a]", "$t =.. [f, a]") } +
                listOf(
                    "T =.. [f, a, b]",
                    "T =.. [a]",
                    "T =.. []",
                    "T =.. [1]",
                    "T =.. [1, 2]",
                    "T =.. [f(a), b]",
                    "T =.. [H, a]",
                    "T =.. L",
                    "T =.. [f | R]",
                    "T =.. ['.', a, b]",
                    "T =.. [',', a, b]",
                    "T =.. [f, X, X], T = f(1, Y), Y == 1",
                ),
        )

    @Test
    fun lengthAndReverse() =
        same(
            terms.flatMap { t ->
                listOf(
                    "length($t, N)",
                    "length($t, 1)",
                    "length($t, 2)",
                    "length($t, -1)",
                    "length($t, a)",
                    "reverse($t, R)",
                    "reverse($t, [a])",
                )
            } +
                listOf(
                    "length(L, 2)",
                    "length(L, 0)",
                    "length([a | T], 3)",
                    "length([a, b], 100000000000)",
                    "length(L, N)",
                    "reverse(L, [a, b])",
                    "reverse(L, R)",
                    "reverse([1, 2, 3], R)",
                    "reverse([1, 2, 3], [3, 2, 1])",
                    "reverse([1, 2, 3], [1, 2, 3])",
                    "reverse([a | T], R)",
                ),
        )

    @Test
    fun atoms() =
        same(
            terms.flatMap { t ->
                listOf(
                    "atom_length($t, N)",
                    "atom_length($t, 1)",
                    "atom_length($t, -1)",
                    "atom_length($t, a)",
                    "atom_concat($t, b, W)",
                    "atom_concat(a, $t, W)",
                    "atom_concat($t, b, ab)",
                    "atom_concat(a, $t, ab)",
                    "atom_concat(a, b, $t)",
                )
            } +
                listOf(
                    "atom_concat(X, Y, ab)",
                    "atom_concat(X, Y, Z)",
                    "atom_concat(X, b, ab)",
                    "atom_concat(a, Y, ab)",
                    "atom_concat(X, c, ab)",
                    "atom_concat(c, Y, ab)",
                    "atom_concat(a, b, ab)",
                    "atom_concat(a, b, c)",
                    "atom_length('', N)",
                    "atom_concat('', '', W)",
                ),
        )

    // The natives stand in for the primitives of the builtins of the solvers: the library is not behind other objects there
    @Test
    fun theNativesStandInForTheDefaultBuiltins() {
        val names = listOf("atom" to 1, "ground" to 1, "==" to 2, "@<" to 2, "functor" to 3, "arg" to 3, "=.." to 2, "copy_term" to 2)
        for ((name, arity) in names) {
            val signature = Signature(name, arity)
            assertNotNull(StdlibNatives.of(signature, DefaultBuiltins.primitives[signature]), "$name/$arity")
        }
    }
}
