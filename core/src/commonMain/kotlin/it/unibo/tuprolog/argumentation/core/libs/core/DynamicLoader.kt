package it.unibo.tuprolog.argumentation.core.libs.core

import it.unibo.tuprolog.argumentation.core.Arg2pSolver
import it.unibo.tuprolog.argumentation.core.libs.ArgLibrary
import it.unibo.tuprolog.argumentation.core.libs.ArgsFlag
import it.unibo.tuprolog.argumentation.core.libs.Loadable
import it.unibo.tuprolog.core.Atom
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.exception.error.TypeError
import it.unibo.tuprolog.solve.library.AliasedLibrary
import it.unibo.tuprolog.solve.library.Library
import it.unibo.tuprolog.solve.primitive.Primitive
import it.unibo.tuprolog.solve.primitive.Solve

object DynamicLoader : ArgLibrary {

    var solver : Arg2pSolver? = null

    override val baseContent: AliasedLibrary
        get() = Library.aliased(
            alias = "prolog.argumentation.loader",
            primitives = mapOf(
                WithLib.signature to WithLib
            ),
        )
    override val baseFlags: Iterable<ArgsFlag<*, *>>
        get() = emptyList()
}

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
            libraries = request.context.libraries.minus(
                DynamicLoader.solver!!.dynamicLibraries()
                    .map { it.content().alias }
                    .filter { request.context.libraries.libraryAliases.contains(it) }
                )
                .plus( DynamicLoader.solver!!.dynamicLibraries().first { (it as Loadable).identifier() == lib.toString()}.content())
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
