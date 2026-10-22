package it.unibo.tuprolog.argumentation.core.unit


import it.unibo.tuprolog.argumentation.core.Arg2pSolverFactory
import it.unibo.tuprolog.argumentation.core.libs.ArgLibrary
import it.unibo.tuprolog.argumentation.core.libs.ArgsFlag
import it.unibo.tuprolog.argumentation.core.runtime.settings.FlagsBuilder
import it.unibo.tuprolog.core.Integer
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Substitution
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.library.Library
import it.unibo.tuprolog.solve.primitive.Primitive
import it.unibo.tuprolog.solve.primitive.Solve
import kotlin.test.Test
import kotlin.test.assertEquals

/**
 * Errors raised by the code of primitives, and by throw/1, are Prolog errors catch/3 can handle, rather than
 * exceptions crashing the resolution: also those raised computing a further answer of a primitive, on backtracking.
 */
class EngineErrorsTest {
    // failing/0 raises a Kotlin exception; flaky/1 answers 1, then raises one computing its second answer
    private object Faulty : ArgLibrary {
        override val alias = "test.faulty"

        private val failing =
            object : Primitive {
                override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> = error("broken primitive")
            }

        private val flaky =
            object : Primitive {
                override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> =
                    sequence {
                        val one = Substitution.of(request.arguments[0].castToVar(), Integer.of(1))
                        yield(request.replySuccess(one))
                        error("broken second answer")
                    }
            }

        override val baseContent: Library
            get() = Library.of(alias, primitives = mapOf(Signature("failing", 0) to failing, Signature("flaky", 1) to flaky))

        override val baseFlags: Iterable<ArgsFlag<*, *>>
            get() = emptyList()
    }

    private fun solve(goal: String): String {
        val solver = Arg2pSolverFactory.default(settings = FlagsBuilder().empty(true).create(), staticLibs = listOf(Faulty))
        val solution = solver.solve(Struct.parse(goal, solver.operators)).first()
        return when {
            solution.isYes ->
                solution.substitution.entries
                    .filter { it.key.name == "L" }
                    .joinToString { it.value.toString() }
            solution.isHalt -> "error"
            else -> "no"
        }
    }

    @Test
    fun throwingAnErrorOfAnyShape() {
        assertEquals("error(1)", solve("catch(throw(error(1)), E, true), L = E"))
        assertEquals("error", solve("throw(error(_, c))"))
    }

    @Test
    fun aPrimitiveRaisingAnException() {
        assertEquals("caught", solve("catch(failing, error(system_error, _), L = caught)"))
        assertEquals("error", solve("failing"))
    }

    @Test
    fun aPrimitiveRaisingAnExceptionOnBacktracking() {
        assertEquals("[1, caught]", solve("findall(X, catch(flaky(X), error(system_error, _), X = caught), L)"))
    }
}
