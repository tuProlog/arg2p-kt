package it.unibo.tuprolog.argumentation.core.libs

import it.unibo.tuprolog.argumentation.core.Arg2p
import it.unibo.tuprolog.argumentation.core.ArgLibraries
import it.unibo.tuprolog.core.Atom
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.classic.ClassicSolverFactory
import it.unibo.tuprolog.solve.exception.error.TypeError
import it.unibo.tuprolog.solve.library.AliasedLibrary
import it.unibo.tuprolog.solve.library.Libraries
import it.unibo.tuprolog.solve.library.Library
import it.unibo.tuprolog.solve.primitive.Primitive
import it.unibo.tuprolog.solve.primitive.Solve

object DynamicLoader : AliasedLibrary by
Library.aliased(
    alias = "prolog.argumentation.loader",
    primitives = mapOf(
        WithLib.signature to WithLib
    )
)

object WithLib : Primitive {

    val signature = Signature("with_lib", 2)

    override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
        val lib: Term = request.arguments[0]
        val goal: Term = request.arguments[1]

        if (lib !is Atom) {
            throw TypeError.forGoal(
                request.context,
                request.signature,
                TypeError.Expected.LIST,
                lib
            )
        }

        if (goal !is Struct) {
            throw TypeError.forGoal(
                request.context,
                request.signature,
                TypeError.Expected.CALLABLE,
                goal
            )
        }

        val solver = request.context.createMutableSolver(
            libraries = request.context.libraries.minus(ArgLibraries.values()
                    .map { it.identifier() }
                    .filter { request.context.libraries.libraryAliases.contains(it) }
                )
                .plus(ArgLibraries.fromIdentifier(lib.toString()))
        )

        return sequence {
            yieldAll(
                solver.solve(goal).map {
                    request.replyWith(it.substitution)
                }
            )
        }
    }
}
