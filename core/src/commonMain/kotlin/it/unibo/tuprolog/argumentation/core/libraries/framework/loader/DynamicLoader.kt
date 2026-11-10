package it.unibo.tuprolog.argumentation.core.libraries.framework.loader

import it.unibo.tuprolog.argumentation.core.Arg2pSolver
import it.unibo.tuprolog.argumentation.core.libraries.ArgLibrary
import it.unibo.tuprolog.argumentation.core.libraries.framework.loader.helpers.LoadedLibraries
import it.unibo.tuprolog.argumentation.core.libraries.framework.loader.helpers.LoadedLibrary
import it.unibo.tuprolog.argumentation.core.libraries.framework.loader.notation.ModuleNotation
import it.unibo.tuprolog.argumentation.core.libraries.framework.loader.primitives.ExtensionFlag
import it.unibo.tuprolog.argumentation.core.libraries.framework.loader.primitives.ModuleDefines
import it.unibo.tuprolog.argumentation.core.libraries.framework.loader.primitives.ModuleGoal
import it.unibo.tuprolog.argumentation.core.libraries.framework.loader.primitives.ModuleGoalInBranch
import it.unibo.tuprolog.argumentation.engine.words.locked
import it.unibo.tuprolog.core.operators.OperatorSet
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.MutableSolver
import it.unibo.tuprolog.solve.library.Library
import it.unibo.tuprolog.solve.primitive.Solve
import it.unibo.tuprolog.theory.Theory

internal class DynamicLoader(
    private val solver: Arg2pSolver,
) : ArgLibrary {
    /** The modules loaded so far */
    internal val modules = LoadedLibraries(solver)

    // The solvers of the modules, for the engines solving module calls through solve (the arg2p one inlines them)
    private val moduleSolvers = mutableMapOf<String, MutableSolver>()

    // The static knowledge base each module solver was last given: loading it again re-indexes all its clauses
    private val loadedKb = mutableMapOf<String, Theory>()

    /** The solver of [module], for the engines solving module calls through solve (the arg2p one inlines them) */
    internal fun solverOf(
        request: Solve.Request<ExecutionContext>,
        module: LoadedLibrary,
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

    override val alias = "prolog.argumentation.loader"

    override val baseContent: Library
        get() =
            listOf(ModuleGoal(this), ModuleGoalInBranch(this), ModuleDefines(this), ExtensionFlag(solver)).let {
                Library.of(
                    alias = this.alias,
                    primitives = it.associateBy { prim -> prim.signature },
                    operators = theoryOperators,
                )
            }

    // The calls of the modules, and the notations of the theories the modules read (see Loadable.notation): the solver
    // reads its theory with them
    override val theoryOperators: OperatorSet by lazy {
        notationOf(
            solver
                .dynamicLibraries()
                .map { it.notation }
                .filterNot { it.isEmpty() }
                .distinct(),
        )
    }

    private companion object {
        // The operators of each set of notations, the calls among them: few sets, the same for every solver
        private val notations = HashMap<List<OperatorSet>, OperatorSet>()

        fun notationOf(sets: List<OperatorSet>): OperatorSet =
            locked(notations) {
                notations.getOrPut(sets) { sets.fold(ModuleNotation.calls) { operators, notation -> operators + notation } }
            }
    }
}
