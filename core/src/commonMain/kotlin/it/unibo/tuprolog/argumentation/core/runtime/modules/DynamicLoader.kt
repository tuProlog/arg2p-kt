package it.unibo.tuprolog.argumentation.core.runtime.modules

import it.unibo.tuprolog.argumentation.core.Arg2pSolver
import it.unibo.tuprolog.argumentation.core.flags.ArgsFlag
import it.unibo.tuprolog.argumentation.core.modules.ArgLibrary
import it.unibo.tuprolog.argumentation.core.modules.ArgLoader
import it.unibo.tuprolog.argumentation.core.primitives.modules.LoaderReset
import it.unibo.tuprolog.argumentation.core.primitives.modules.ModuleGoal
import it.unibo.tuprolog.argumentation.core.primitives.modules.ModuleGoalInBranch
import it.unibo.tuprolog.core.operators.Operator
import it.unibo.tuprolog.core.operators.OperatorSet
import it.unibo.tuprolog.core.operators.Specifier
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.MutableSolver
import it.unibo.tuprolog.solve.library.Library
import it.unibo.tuprolog.solve.primitive.Solve
import it.unibo.tuprolog.theory.Theory

internal class DynamicLoader(
    solver: Arg2pSolver,
) : ArgLibrary,
    ArgLoader {
    /** The modules loaded so far */
    internal val modules = Modules(solver)

    // The solvers of the modules, for the engines solving module calls through solve (the arg2p one inlines them)
    private val moduleSolvers = mutableMapOf<String, MutableSolver>()

    // The static knowledge base each module solver was last given: loading it again re-indexes all its clauses
    private val loadedKb = mutableMapOf<String, Theory>()

    /** The solver of [module], for the engines solving module calls through solve (the arg2p one inlines them) */
    internal fun solverOf(
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

    /** Forgets the modules loaded so far, and their solvers */
    internal fun reset() {
        modules.clear()
        moduleSolvers.clear()
        loadedKb.clear()
    }

    override val alias = "prolog.argumentation.loader"

    override val baseContent: Library
        get() =
            listOf(ModuleGoal(this), ModuleGoalInBranch(this), LoaderReset(this)).let {
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
