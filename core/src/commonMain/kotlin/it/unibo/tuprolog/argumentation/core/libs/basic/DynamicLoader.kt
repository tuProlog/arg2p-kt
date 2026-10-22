package it.unibo.tuprolog.argumentation.core.libs.basic

import it.unibo.tuprolog.argumentation.core.Arg2pSolver
import it.unibo.tuprolog.argumentation.core.engine.DelegatedCall
import it.unibo.tuprolog.argumentation.core.engine.Delegation
import it.unibo.tuprolog.argumentation.core.engine.Inlining
import it.unibo.tuprolog.argumentation.core.libs.ArgLibrary
import it.unibo.tuprolog.argumentation.core.libs.ArgLoader
import it.unibo.tuprolog.argumentation.core.libs.ArgsFlag
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
import it.unibo.tuprolog.solve.Solution
import it.unibo.tuprolog.solve.SolveOptions
import it.unibo.tuprolog.solve.TimeDuration
import it.unibo.tuprolog.solve.exception.error.TypeError
import it.unibo.tuprolog.solve.library.Library
import it.unibo.tuprolog.solve.primitive.Solve
import it.unibo.tuprolog.theory.Theory

class DynamicLoader(
    solver: Arg2pSolver,
) : ArgLibrary,
    ArgLoader {
    private val modules = Modules(solver)

    // The solvers of the modules, for the engines solving module calls through solve (the arg2p one inlines them)
    private val moduleSolvers = mutableMapOf<String, MutableSolver>()

    // The static knowledge base each module solver was last given: loading it again re-indexes all its clauses
    private val loadedKb = mutableMapOf<String, Theory>()

    private fun solverOf(
        request: Solve.Request<ExecutionContext>,
        module: Module,
    ): MutableSolver =
        moduleSolvers
            .getOrPut(module.name) { request.context.createMutableSolver(libraries = module.libraries) }
            .also {
                val staticKb = request.context.staticKb
                if (loadedKb[module.name] !== staticKb) {
                    it.loadStaticKb(staticKb)
                    loadedKb[module.name] = staticKb
                }
            }

    // Module calls are inlined by the arg2p engine (DelegatedCall): the goal is solved in place, against the libraries
    // of the module. Other solvers solve it in a solver of the module, cached.
    abstract inner class AbstractWithLib :
        PrimitiveWithSignature,
        DelegatedCall {
        abstract override val signature: Signature

        /** What the call does before its goal is solved, in [module] */
        protected abstract fun enter(
            request: Solve.Request<ExecutionContext>,
            module: Module,
        )

        // The module and the goal of the call, checked
        private fun call(request: Solve.Request<ExecutionContext>): Pair<Atom, Struct> {
            val lib: Term = request.arguments[0]
            val goal: Term = request.arguments[1]

            if (lib !is Atom) {
                throw TypeError.forGoal(
                    request.context,
                    request.signature,
                    TypeError.Expected.ATOM,
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
            return lib to goal
        }

        override fun delegate(request: Solve.Request<ExecutionContext>): Delegation {
            val (name, goal) = call(request)
            val module = modules.of(request, name)
            enter(request, module)
            return Inlining(goal, module.libraries)
        }

        override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
            val (name, goal) = call(request)
            val module = modules.of(request, name)
            enter(request, module)
            val solver = solverOf(request, module)
            return sequence {
                yieldAll(
                    solver.solveForCaller(goal, SolveOptions.allLazilyWithTimeout(TimeDuration.MAX_VALUE)).map {
                        when (it) {
                            // Halt carries a failed substitution, so replying with it would turn an
                            // error inside the module into an ordinary failure: indistinguishable
                            // from a goal that simply does not hold.
                            is Solution.Halt -> throw it.exception
                            else -> request.replyWith(it.substitution)
                        }
                    },
                )
            }
        }
    }

    inner class WithLib : AbstractWithLib() {
        override val signature = Signature("::", 2)

        override fun enter(
            request: Solve.Request<ExecutionContext>,
            module: Module,
        ) = Unit
    }

    inner class WithLibInNewContext : AbstractWithLib() {
        override val signature = Signature(":::", 2)

        // A new branch of the active one, through the context library if it is reachable, otherwise solving the goals
        // of the context in a solver of the module
        override fun enter(
            request: Solve.Request<ExecutionContext>,
            module: Module,
        ) {
            val context = Context.of(module.libraries)
            if (context != null) {
                context.branch(context.active)
                return
            }
            val solver = request.context.createSolver(libraries = module.libraries)
            logicProgramming {
                solver
                    .solve("context_active"(X))
                    .filter { it.isYes }
                    .forEach {
                        solver.solve("context_branch"(it.substitution[X]!!, `_`)).first()
                    }
            }
        }
    }

    inner class ResetLoader : PrimitiveWithSignature {
        override val signature = Signature("loader_reset", 0)

        override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
            this@DynamicLoader.modules.clear()
            this@DynamicLoader.moduleSolvers.clear()
            this@DynamicLoader.loadedKb.clear()
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
