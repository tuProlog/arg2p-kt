package it.unibo.tuprolog.argumentation.core.libs.basic

import it.unibo.tuprolog.argumentation.core.Arg2pSolver
import it.unibo.tuprolog.argumentation.core.libs.ArgLibrary
import it.unibo.tuprolog.argumentation.core.libs.ArgLoader
import it.unibo.tuprolog.argumentation.core.libs.ArgsFlag
import it.unibo.tuprolog.argumentation.core.libs.Loadable
import it.unibo.tuprolog.core.Atom
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.operators.Operator
import it.unibo.tuprolog.core.operators.OperatorSet
import it.unibo.tuprolog.core.operators.Specifier
import it.unibo.tuprolog.dsl.prolog
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.MutableSolver
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.exception.error.DomainError
import it.unibo.tuprolog.solve.exception.error.TypeError
import it.unibo.tuprolog.solve.library.AliasedLibrary
import it.unibo.tuprolog.solve.library.Libraries
import it.unibo.tuprolog.solve.library.Library
import it.unibo.tuprolog.solve.primitive.Primitive
import it.unibo.tuprolog.solve.primitive.Solve

class DynamicLoader(private val solver: Arg2pSolver) : ArgLibrary, ArgLoader {

    abstract inner class AbstractWithLib : Primitive {

        abstract val signature: Signature
        abstract fun execute(module: String, solver: MutableSolver)

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

            val solver = (
                this@DynamicLoader.solver.dynamicLibraries().firstOrNull {
                    (it as Loadable).identifier() == lib.toString()
                } ?: throw DomainError.forGoal(
                    request.context,
                    request.signature,
                    DomainError.Expected.of("Loadable Lib"),
                    lib
                )
                ).let { library ->
                request.context.createMutableSolver(
                    libraries = Libraries.of(
                        request.context.libraries.libraries.filterNot { lib ->
                            this@DynamicLoader.solver.dynamicLibraries()
                                .map { it.alias }
                                .contains(lib.alias)
                        }
                    ).plus(library.content()),
                    staticKb = request.context.staticKb
                )
            }

            execute(lib.toString(), solver)

            return sequence {
                yieldAll(
                    solver.solve(goal).map {
                        request.replyWith(it.substitution)
                    }
                )
            }
        }
    }

    inner class WithLib : AbstractWithLib() {
        override val signature = Signature("::", 2)
        override fun execute(module: String, solver: MutableSolver) = Unit
    }

    inner class WithLibInNewContext : AbstractWithLib() {
        override val signature = Signature(":::", 2)
        override fun execute(module: String, solver: MutableSolver) =
            prolog {
                solver.solve("context_active"(X))
                    .filter { it.isYes }
                    .forEach {
                        solver.solve("context_branch"(it.substitution[X]!!, `_`)).first()
                    }
            }
    }

    override val alias = "prolog.argumentation.loader"

    override val baseContent: AliasedLibrary
        get() = listOf(WithLib(), WithLibInNewContext()).let {
            Library.aliased(
                alias = this.alias,
                primitives = it.associateBy { prim -> prim.signature },
                operatorSet = operators()
            )
        }
    override val baseFlags: Iterable<ArgsFlag<*, *>>
        get() = emptyList()

    override var theoryOperators = operators()

    companion object {
        fun operators() = OperatorSet(
            Operator("::", Specifier.XFX, 700),
            Operator(":::", Specifier.XFX, 700),
        )
    }
}
