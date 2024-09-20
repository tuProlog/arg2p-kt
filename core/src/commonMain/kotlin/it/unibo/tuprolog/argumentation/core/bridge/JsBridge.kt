package it.unibo.tuprolog.argumentation.core.bridge

import it.unibo.tuprolog.argumentation.core.Arg2pSolver
import it.unibo.tuprolog.argumentation.core.dsl.arg2pScope
import it.unibo.tuprolog.argumentation.core.mining.graph
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.solve.MutableSolver
import it.unibo.tuprolog.solve.Solver
import it.unibo.tuprolog.solve.channel.OutputChannel
import it.unibo.tuprolog.solve.classic.ClassicSolverFactory
import it.unibo.tuprolog.solve.flags.TrackVariables
import it.unibo.tuprolog.solve.flags.Unknown
import it.unibo.tuprolog.theory.parsing.parse
import kotlin.js.JsExport

@JsExport
data class BridgedArgument(val id: String, val descriptor: String, val label: String)

@JsExport
data class BridgedAttack(val from: String, val to: String)

@JsExport
data class JsPair<A, B>(val first: A, val second: B)

@JsExport
class JsIterator<T>(private val nextFunction: () -> T?, private val hasNextFunction: () -> Boolean) {
    fun hasNext(): Boolean = hasNextFunction()

    fun next(): T? {
        if (!hasNext()) throw NoSuchElementException("No more elements")
        return nextFunction()
    }
}

@JsExport
data class BridgedResult(val i: JsIterator<BridgedSolution>, val query: String)

@JsExport
data class BridgedGraph(val arguments: Array<BridgedArgument>, val attacks: Array<BridgedAttack>) {
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
    private fun graphOf(solver: MutableSolver): BridgedGraph {
        val graph =
            solver.graph(
                arg2pScope {
                    solver.solve("context_active"(X))
                        .filter { it.isYes }
                        .map { it.substitution[X].toString().toInt() }
                        .first()
                },
            )

        return BridgedGraph(
            arguments =
                graph.labellings
                    .map { BridgedArgument(it.argument.identifier, it.argument.descriptor, it.label) }.toTypedArray(),
            attacks =
                graph.attacks
                    .map { BridgedAttack(it.attacker.identifier, it.target.identifier) }.toTypedArray(),
        )
    }

    private fun solverOf(
        outputConsumer: (String) -> Unit,
        flagsText: String,
    ): MutableSolver {
        val flags = it.unibo.tuprolog.theory.Theory.parse(flagsText)
        val libFlags = it.unibo.tuprolog.solve.library.Library.of(alias = "argumentation.flags", clauses = flags)

        return Solver.prolog.mutableSolverWithDefaultBuiltins(
            otherLibraries = ClassicSolverFactory.defaultRuntime.plus(Arg2pSolver.default().to2pLibraries().plus(libFlags)),
            flags = ClassicSolverFactory.defaultFlags.set(Unknown, Unknown.FAIL).set(TrackVariables, TrackVariables.ON),
            staticKb = it.unibo.tuprolog.theory.Theory.empty(),
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
        val query = it.unibo.tuprolog.core.Struct.parse(queryText, solver.operators)
        val theory = it.unibo.tuprolog.theory.Theory.parse(theoryText, solver.operators)
        val formatter = it.unibo.tuprolog.core.TermFormatter.prettyExpressions()
        solver.loadStaticKb(theory)

        val solutions =
            solver.solve(query).map { sol ->
                if (sol.isYes) {
                    BridgedSolution(
                        "yes",
                        formatter.format(sol.solvedQuery!!),
                        sol.substitution
                            .map { JsPair(formatter.format(it.key), formatter.format(it.value)) }.toTypedArray(),
                        "",
                        graphOf(solver),
                    )
                } else if (sol.isNo) {
                    BridgedSolution("no", "", emptyArray(), "", graphOf(solver))
                } else {
                    BridgedSolution("halt", "", emptyArray(), sol.exception.toString(), graphOf(solver))
                }
            }

        val i = solutions.iterator()
        val jsIt =
            JsIterator(
                nextFunction = { if (i.hasNext()) i.next() else null },
                hasNextFunction = { i.hasNext() },
            )

        return BridgedResult(jsIt, formatter.format(query))
    }
}
