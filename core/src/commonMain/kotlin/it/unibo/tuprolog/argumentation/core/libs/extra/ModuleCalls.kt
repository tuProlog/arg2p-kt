package it.unibo.tuprolog.argumentation.core.libs.extra

import it.unibo.tuprolog.argumentation.core.libs.ArgLibrary
import it.unibo.tuprolog.argumentation.core.libs.ArgsFlag
import it.unibo.tuprolog.argumentation.core.libs.Loadable
import it.unibo.tuprolog.argumentation.core.libs.language.RuleParserBase
import it.unibo.tuprolog.core.List
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.operators.OperatorSet
import it.unibo.tuprolog.dsl.theory.logicProgramming
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.MutableSolver
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.Solution
import it.unibo.tuprolog.solve.Solver
import it.unibo.tuprolog.solve.classic.ClassicSolverFactory
import it.unibo.tuprolog.solve.exception.error.ExistenceError
import it.unibo.tuprolog.solve.exception.error.TypeError
import it.unibo.tuprolog.solve.library.Library
import it.unibo.tuprolog.solve.libs.io.IOLib
import it.unibo.tuprolog.solve.libs.io.Url
import it.unibo.tuprolog.solve.primitive.Primitive
import it.unibo.tuprolog.solve.primitive.Solve
import it.unibo.tuprolog.theory.Theory
import it.unibo.tuprolog.theory.parsing.parse

object ModuleCalls : ArgLibrary, Loadable {
    override val alias = "prolog.argumentation.modularity"

    override val baseContent: Library
        get() =
            Library.of(
                alias = this.alias,
                primitives =
                    mapOf(
                        ModuleCall.signature to ModuleCall,
                    ),
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
                modules,
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

        val solver = getCleanSolver(request.context, mineModulesPath(request.context), modules.toList().map { it.toString() })
        return sequence {
            yieldAll(
                solver.solve(goal).map {
                    when (it) {
                        is Solution.Yes -> request.replySuccess(it.substitution)
                        else -> request.replyFail()
                    }
                },
            )
        }
    }

    private fun mineModulesPath(context: ExecutionContext): String =
        logicProgramming {
            ClassicSolverFactory
                .solverOf(libraries = context.libraries)
                .solve("modulesPath"(X))
                .map { if (it is Solution.Yes) it.substitution[X].toString() else "" }
                .first()
        }

    private fun getCleanSolver(
        context: ExecutionContext,
        modulesPath: String,
        modules: Iterable<String>,
    ): Solver {
        val module = { mod: String ->
            // Module names arrive as terms, so an atom that needs quoting still carries its quotes here.
            val name = mod.removeSurrounding("'")
            // Anything carrying a scheme, or naming a file, is a location: everything else is a module
            // name to be resolved against modulesPath.
            val isLocation = name.contains("://") || name.contains(".pl")
            asFileUrl(
                if (isLocation) name else "${modulesPath.removeSurrounding("'")}/$name.pl",
            )
        }
        // consult/1 comes from IOLib, which is not part of the argumentation runtime: without it every
        // module load would silently fail, and any goal depending on the module would fail as well.
        // Built from the calling context so that its flags travel with it: the argumentation theories rely
        // on unknown predicates failing rather than raising, which the default flag set does not provide.
        return logicProgramming {
            context
                .createMutableSolver(
                    libraries = context.libraries + IOLib,
                    // The module is evaluated on its own content, without the surrounding theory.
                    staticKb = Theory.empty(),
                ).also { solver: MutableSolver ->
                    modules.forEach { mod ->
                        val target = module(mod)
                        // Read and parse the module here rather than through consult/1: the latter parses
                        // with the operators of the calling context, which do not include the arg2p ones
                        // when the primitive is reached through the :: dispatch, so a theory written in
                        // arg2p syntax would fail to load.
                        val theory =
                            runCatching {
                                Theory.parse(
                                    Url.of(target).readAsText(),
                                    RuleParserBase.operators().plus(OperatorSet.DEFAULT),
                                )
                            }.getOrNull() ?: throw ExistenceError.forSourceSink(context, target)
                        solver.loadStaticKb(theory)
                    }
                }
        }
    }

    // consult/1 resolves its argument as a URL: a bare filesystem path is rejected, and on Windows it is
    // read as a URI whose scheme is the drive letter. Absolute paths therefore need an explicit file scheme.
    private fun asFileUrl(path: String): String {
        val normalized = path.replace('\\', '/')
        return when {
            normalized.contains("://") -> normalized
            normalized.startsWith("/") -> "file://$normalized"
            else -> "file:///$normalized"
        }
    }
}
