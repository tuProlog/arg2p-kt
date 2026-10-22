package it.unibo.tuprolog.argumentation.bridge

import it.unibo.tuprolog.argumentation.causality.libs.CausalitySolver
import it.unibo.tuprolog.argumentation.core.Arg2pSolver
import it.unibo.tuprolog.argumentation.core.dsl.arg2pScope
import it.unibo.tuprolog.argumentation.core.engine.Arg2pPrologSolverFactory
import it.unibo.tuprolog.argumentation.core.libs.basic.Arg2pUnificator
import it.unibo.tuprolog.argumentation.core.mining.graph
import it.unibo.tuprolog.core.Integer
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.TermFormatter
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.solve.MutableSolver
import it.unibo.tuprolog.solve.channel.OutputChannel
import it.unibo.tuprolog.solve.flags.TrackVariables
import it.unibo.tuprolog.solve.flags.Unknown
import it.unibo.tuprolog.solve.library.Library
import it.unibo.tuprolog.theory.Theory
import it.unibo.tuprolog.theory.parsing.parse
import kotlin.js.JsExport

@JsExport
data class BridgedArgument(
    val id: String,
    val descriptor: String,
    val label: String,
)

@JsExport
data class BridgedAttack(
    val from: String,
    val to: String,
)

@JsExport
data class JsPair<A, B>(
    val first: A,
    val second: B,
)

@JsExport
class JsIterator<T>(
    private val nextFunction: () -> T?,
    private val hasNextFunction: () -> Boolean,
) {
    fun hasNext(): Boolean = hasNextFunction()

    fun next(): T? {
        if (!hasNext()) throw NoSuchElementException("No more elements")
        return nextFunction()
    }
}

/**
 * The solutions of a query, and the context of the solver that computed them. The context keeps a branch for every
 * stage of the computation (each module call made with :::), numbered from 0 to the context of a solution: they can
 * be inspected while, and after, iterating the solutions.
 */
@JsExport
class BridgedResult internal constructor(
    val i: JsIterator<BridgedSolution>,
    val query: String,
    private val solver: MutableSolver,
    private val formatter: TermFormatter,
) {
    /** The graph stored in the given context branch */
    fun graphAt(context: Int): BridgedGraph = JsBridge.graphOf(solver, context)

    /**
     * The solutions of context_check(Context, Goal): the clauses stored in a context branch matching Goal (facts, or
     * clause(Head, Body) for rules), each as the bindings of the variables of Goal
     */
    fun queryContext(
        context: Int,
        goal: String,
    ): Array<Array<JsPair<String, String>>> {
        val term = Struct.parse(goal, solver.operators)
        val variables = term.variables.distinct().toList()
        return solver
            .solve(Struct.of("context_check", Integer.of(context), term))
            .filter { it.isYes }
            .map { sol ->
                // keyed by the variable names written in the goal
                variables
                    .mapNotNull { v -> sol.substitution[v]?.let { JsPair(v.name, formatter.format(it)) } }
                    .toTypedArray()
            }.toList()
            .toTypedArray()
    }
}

@JsExport
data class BridgedGraph(
    val arguments: Array<BridgedArgument>,
    val attacks: Array<BridgedAttack>,
) {
    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (other == null || this::class != other::class) return false

        other as BridgedGraph

        if (!arguments.contentEquals(other.arguments)) return false
        if (!attacks.contentEquals(other.attacks)) return false

        return true
    }

    override fun hashCode(): Int {
        var result = arguments.contentHashCode()
        result = 31 * result + attacks.contentHashCode()
        return result
    }
}

@JsExport
data class BridgedSolution(
    val res: String,
    val query: String,
    val substitutions: Array<JsPair<String, String>>,
    val exception: String,
    val graph: BridgedGraph,
    // The context branch the graph was mined from: the last stage of the computation
    val context: Int = 0,
) {
    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (other == null || this::class != other::class) return false

        other as BridgedSolution

        if (res != other.res) return false
        if (query != other.query) return false
        if (!substitutions.contentEquals(other.substitutions)) return false
        if (exception != other.exception) return false

        return true
    }

    override fun hashCode(): Int {
        var result = res.hashCode()
        result = 31 * result + query.hashCode()
        result = 31 * result + substitutions.contentHashCode()
        result = 31 * result + exception.hashCode()
        return result
    }
}

@JsExport
object JsBridge {
    private fun activeContext(solver: MutableSolver): Int =
        arg2pScope {
            solver
                .solve("context_active"(X))
                .filter { it.isYes }
                .map { it.substitution[X].toString().toInt() }
                .first()
        }

    internal fun graphOf(
        solver: MutableSolver,
        context: Int,
    ): BridgedGraph {
        val graph = solver.graph(context)

        return BridgedGraph(
            arguments =
                graph.labellings
                    .map { BridgedArgument(it.argument.identifier, it.argument.descriptor, it.label) }
                    .toTypedArray(),
            attacks =
                graph.attacks
                    .map { BridgedAttack(it.attacker.identifier, it.target.identifier) }
                    .toTypedArray(),
        )
    }

    private fun solverOf(
        outputConsumer: (String) -> Unit,
        flagsText: String,
    ): MutableSolver {
        val flags = Theory.parse(flagsText)
        val libFlags = Library.of(alias = "argumentation.flags", clauses = flags)

        return Arg2pPrologSolverFactory.mutableSolverWithDefaultBuiltins(
            // the same unificator as the solvers built by Arg2pSolverFactory: lookups match clause heads only
            unificator = Arg2pUnificator,
            otherLibraries =
                Arg2pPrologSolverFactory.defaultRuntime.plus(
                    Arg2pSolver
                        .default(
                            dynamicLibs = listOf(CausalitySolver()),
                        ).to2pLibraries()
                        .plus(libFlags),
                ),
            flags = Arg2pPrologSolverFactory.defaultFlags.set(Unknown, Unknown.FAIL).set(TrackVariables, TrackVariables.ON),
            staticKb = Theory.empty(),
            stdOut = OutputChannel.of(outputConsumer),
            stdErr = OutputChannel.of { _ -> },
            warnings = OutputChannel.of { _ -> },
        )
    }

    fun solve(
        queryText: String,
        theoryText: String,
        flagsText: String,
        outputConsumer: (String) -> Unit,
    ): BridgedResult {
        val solver = solverOf(outputConsumer, flagsText)
        val query = Struct.parse(queryText, solver.operators)
        val theory = Theory.parse(theoryText, solver.operators)
        val formatter = TermFormatter.prettyExpressions()
        solver.loadStaticKb(theory)

        val solutions =
            solver.solve(query).map { sol ->
                val context = activeContext(solver)
                val graph = graphOf(solver, context)
                if (sol.isYes) {
                    BridgedSolution(
                        "yes",
                        formatter.format(sol.solvedQuery!!),
                        sol.substitution
                            .map { JsPair(formatter.format(it.key), formatter.format(it.value)) }
                            .toTypedArray(),
                        "",
                        graph,
                        context,
                    )
                } else if (sol.isNo) {
                    BridgedSolution("no", "", emptyArray(), "", graph, context)
                } else {
                    BridgedSolution("halt", "", emptyArray(), sol.exception.toString(), graph, context)
                }
            }

        val i = solutions.iterator()
        val jsIt =
            JsIterator<BridgedSolution>(
                nextFunction = { if (i.hasNext()) i.next() else null },
                hasNextFunction = { i.hasNext() },
            )

        return BridgedResult(jsIt, formatter.format(query), solver, formatter)
    }
}
