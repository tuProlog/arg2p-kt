package it.unibo.tuprolog.argumentation.core.libs.extra

import it.unibo.tuprolog.argumentation.core.libs.ArgLibrary
import it.unibo.tuprolog.argumentation.core.libs.ArgsFlag
import it.unibo.tuprolog.argumentation.core.libs.Loadable
import it.unibo.tuprolog.core.List
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.dsl.theory.prolog
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.Solution
import it.unibo.tuprolog.solve.Solver
import it.unibo.tuprolog.solve.classic.classic
import it.unibo.tuprolog.solve.exception.error.TypeError
import it.unibo.tuprolog.solve.library.AliasedLibrary
import it.unibo.tuprolog.solve.library.Library
import it.unibo.tuprolog.solve.primitive.Primitive
import it.unibo.tuprolog.solve.primitive.Solve

object ModuleCalls : ArgLibrary, Loadable {
    override val baseContent: AliasedLibrary
        get() = Library.aliased(
            alias = "prolog.argumentation.modularity",
            primitives = mapOf(
                ModuleCall.signature to ModuleCall
            )
        )
    override val baseFlags: Iterable<ArgsFlag<*, *>>
        get() = listOf(ModulesPath)

    override fun identifier(): String = "module"
}

object ModulesPath : ArgsFlag<String, Unit> {
    override fun predicate(): String = "modulesPath"
    override fun default(): String = "none"
    override fun values() {}
}

object ModuleCall : Primitive {

    val signature = Signature("call_module", 2)

    override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
        val modules: Term = request.arguments[0]
        val goal: Term = request.arguments[1]

        if (modules !is List) {
            throw TypeError.forGoal(
                request.context,
                request.signature,
                TypeError.Expected.LIST,
                modules
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

        val solver = getCleanSolver(request.context, mineModulesPath(request.context), modules.toList().map { it.toString() })
        return sequence {
            yieldAll(
                solver.solve(goal).map {
                    when (it) {
                        is Solution.Yes -> request.replySuccess(it.substitution)
                        else -> request.replyFail()
                    }
                }
            )
        }
    }

    private fun mineModulesPath(context: ExecutionContext): String {
        return prolog {
            Solver.classic(libraries = context.libraries)
                .solve("modulesPath"(X))
                .map { if (it is Solution.Yes) it.substitution[X].toString() else "" }
                .first()
        }
    }

    private fun getCleanSolver(context: ExecutionContext, modulesPath: String, modules: Iterable<String>): Solver {
        val module = { mod: String -> if (mod.contains(".pl")) mod else "${modulesPath.removeSurrounding("'")}/$mod.pl" }
        return prolog {
            Solver.classic(
                libraries = context.libraries
            ).also { solver: Solver -> modules.forEach { solver.solve("consult"(module(it))).toList() } }
        }
    }
}
