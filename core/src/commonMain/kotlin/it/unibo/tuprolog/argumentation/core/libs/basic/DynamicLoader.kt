package it.unibo.tuprolog.argumentation.core.libs.basic

import it.unibo.tuprolog.argumentation.core.Arg2pSolver
import it.unibo.tuprolog.argumentation.core.libs.ArgLibrary
import it.unibo.tuprolog.argumentation.core.libs.ArgLoader
import it.unibo.tuprolog.argumentation.core.libs.ArgsFlag
import it.unibo.tuprolog.argumentation.core.libs.Loadable
import it.unibo.tuprolog.argumentation.core.libs.PrimitiveWithSignature
import it.unibo.tuprolog.core.Atom
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.operators.Operator
import it.unibo.tuprolog.core.operators.OperatorSet
import it.unibo.tuprolog.core.operators.Specifier
import it.unibo.tuprolog.dsl.logicProgramming
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.MutableSolver
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.SolveOptions
import it.unibo.tuprolog.solve.TimeDuration
import it.unibo.tuprolog.solve.exception.error.DomainError
import it.unibo.tuprolog.solve.exception.error.TypeError
import it.unibo.tuprolog.solve.library.Library
import it.unibo.tuprolog.solve.library.Runtime
import it.unibo.tuprolog.solve.primitive.Solve

class DynamicLoader(
    private val solver: Arg2pSolver,
) : ArgLibrary,
    ArgLoader {
    private val solverCache = mutableMapOf<String, MutableSolver>()

    fun loadWithMemo(
        request: Solve.Request<ExecutionContext>,
        library: Term,
    ): MutableSolver =
        solverCache
            .getOrPut(library.toString()) {
                (
                    this.solver.dynamicLibraries().firstOrNull {
                        (it as Loadable).identifier() == library.toString()
                    } ?: throw DomainError.forGoal(
                        request.context,
                        request.signature,
                        DomainError.Expected.of("Loadable Lib"),
                        library,
                    )
                ).let { x ->
                    request.context.createMutableSolver(
                        libraries =
                            Runtime
                                .of(
                                    request.context.libraries.libraries.filterNot { lib ->
                                        this.solver
                                            .dynamicLibraries()
                                            .map { it.alias }
                                            .contains(lib.alias)
                                    },
                                ).plus(x.content()),
                    )
                }
            }.also {
                it.loadStaticKb(request.context.staticKb)
            }

    abstract inner class AbstractWithLib : PrimitiveWithSignature {
        abstract override val signature: Signature

        abstract fun execute(
            module: String,
            solver: MutableSolver,
        )

        override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
            val lib: Term = request.arguments[0]
            val goal: Term = request.arguments[1]

            if (lib !is Atom) {
                throw TypeError.forGoal(
                    request.context,
                    request.signature,
                    TypeError.Expected.LIST,
                    lib,
                )
            }

            if (goal !is Struct) {
                throw TypeError.forGoal(
                    request.context,
                    request.signature,
                    TypeError.Expected.CALLABLE,
                    goal,
                )
            }

            val solver = loadWithMemo(request, lib)

            execute(lib.toString(), solver)

            return sequence {
                yieldAll(
                    solver.solve(goal, SolveOptions.allLazilyWithTimeout(TimeDuration.MAX_VALUE)).map {
                        request.replyWith(it.substitution)
                    },
                )
            }
        }
    }

    inner class WithLib : AbstractWithLib() {
        override val signature = Signature("::", 2)

        override fun execute(
            module: String,
            solver: MutableSolver,
        ) = Unit
    }

    inner class WithLibInNewContext : AbstractWithLib() {
        override val signature = Signature(":::", 2)

        override fun execute(
            module: String,
            solver: MutableSolver,
        ) = logicProgramming {
            solver
                .solve("context_active"(X))
                .filter { it.isYes }
                .forEach {
                    solver.solve("context_branch"(it.substitution[X]!!, `_`)).first()
                }
        }
    }

    inner class ResetLoader : PrimitiveWithSignature {
        override val signature = Signature("loader_reset", 0)

        override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
            this@DynamicLoader.solverCache.clear()
            return sequenceOf(request.replyWith(true))
        }
    }

    override val alias = "prolog.argumentation.loader"

    override val baseContent: Library
        get() =
            listOf(WithLib(), WithLibInNewContext(), ResetLoader()).let {
                Library.of(
                    alias = this.alias,
                    primitives = it.associateBy { prim -> prim.signature },
                    operators = operators(),
                )
            }
    override val baseFlags: Iterable<ArgsFlag<*, *>>
        get() = emptyList()

    override var theoryOperators = operators()

    companion object {
        fun operators() =
            OperatorSet(
                Operator("::", Specifier.XFX, 700),
                Operator(":::", Specifier.XFX, 700),
            )
    }
}
