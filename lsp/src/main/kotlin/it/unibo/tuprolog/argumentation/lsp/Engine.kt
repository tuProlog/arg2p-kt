package it.unibo.tuprolog.argumentation.lsp

import it.unibo.tuprolog.argumentation.Arg2p
import it.unibo.tuprolog.argumentation.Arg2pException
import it.unibo.tuprolog.argumentation.Flags
import it.unibo.tuprolog.argumentation.Solutions
import it.unibo.tuprolog.argumentation.SyntaxProblem
import it.unibo.tuprolog.argumentation.core.model.Graph
import java.util.concurrent.atomic.AtomicInteger

/**
 * The solver of an editor: its runs, the queries it solves one solution at a time, kept until closed (the oldest
 * forgotten past [MAX_RUNS]); what they print goes to [output], with the id of their run.
 */
class Engine(
    private val output: (id: Int, text: String) -> Unit = { _, _ -> },
) {
    private class Run(
        val solver: Arg2p,
        val solutions: Solutions,
        val profile: String,
    )

    private val ids = AtomicInteger()
    private val runs =
        object : LinkedHashMap<Int, Run>() {
            override fun removeEldestEntry(eldest: MutableMap.MutableEntry<Int, Run>): Boolean = size > MAX_RUNS
        }

    /** Starts solving [query] on [theory] with [flags] (Prolog facts) */
    fun solve(
        theory: String,
        query: String,
        flags: String,
    ): SolveAnswer {
        val id = ids.incrementAndGet()
        if (query.isBlank()) return SolveAnswer(id, error = ErrorData("Query error", "Query is a mandatory field."))
        // the flags, the theory and the query are read in turn: the error tells which one it is about
        var reading = "flags"
        return try {
            val parsed = Flags.parse(flags)
            reading = "theory"
            val solver =
                Arg2p
                    .builder()
                    .theory(theory)
                    .flags(parsed)
                    .output { output(id, it) }
                    .build()
            reading = "query"
            val written = query.trim().removeSuffix(".")
            val solutions = solver.solve(written)
            synchronized(runs) { runs[id] = Run(solver, solutions, profileOf(solver)) }
            SolveAnswer(id, written)
        } catch (error: Arg2pException) {
            val name = "${reading.replaceFirstChar { it.uppercase() }} error"
            SolveAnswer(id, error = ErrorData(name, error.message ?: name, error.syntax?.let { problemData(it, reading) }))
        }
    }

    /** The next solution of the run [id] */
    fun next(id: Int): NextAnswer {
        val run = synchronized(runs) { runs[id] } ?: return NextAnswer(error = ErrorData("Error", "This query is no longer running."))
        return try {
            val found = run.solutions.next()
            val (graph, branch) = currentGraph(run)
            NextAnswer(
                SolutionData(
                    result =
                        if (found.isYes) {
                            "yes"
                        } else if (found.isNo) {
                            "no"
                        } else {
                            "halt"
                        },
                    query = found.query,
                    bindings = found.variables.map { BindingData(it, found.text(it) ?: "") },
                    error = found.error,
                    graph = graph,
                    branch = branch,
                ),
                run.solutions.hasNext(),
            )
        } catch (error: Exception) {
            NextAnswer(SolutionData("halt", "", emptyList(), error.message ?: error.toString(), GraphData.EMPTY, 0), false)
        }
    }

    /** The graph of the stage [branch] of the run [id]: none for a stage that cannot be read */
    fun graphAt(
        id: Int,
        branch: Int,
    ): GraphData {
        val run = synchronized(runs) { runs[id] } ?: return GraphData.EMPTY
        return try {
            graphData(run.solver.graphAt(branch), run.profile)
        } catch (_: Exception) {
            GraphData.EMPTY
        }
    }

    fun close(id: Int) {
        synchronized(runs) { runs.remove(id) }
    }

    // The graph of the last stage of the last resolution of [run]: none without one (a plain Prolog goal builds none)
    private fun currentGraph(run: Run): Pair<GraphData, Int> =
        try {
            val branch = run.solver.activeBranch
            graphData(run.solver.graphAt(branch), run.profile) to branch
        } catch (_: Exception) {
            GraphData.EMPTY to 0
        }

    companion object {
        /** How many runs are kept */
        const val MAX_RUNS = 16

        /** Where [text], a theory (or flags) or a query ([kind]), does not parse; null if it does */
        fun check(
            text: String,
            kind: String,
        ): ProblemData? =
            if (kind == "query") {
                if (text.isBlank()) null else Arg2p.checkQuery(text)?.let { problemData(it, null) }
            } else {
                Arg2p.checkTheory(text)?.let { problemData(it, null) }
            }

        /**
         * The values of the flags in effect for [theory] solved with [flags]: those of the flags alone when the theory
         * does not parse, none when they do not
         */
        fun settings(
            theory: String,
            flags: String,
        ): Map<String, List<String>> {
            val parsed =
                try {
                    Flags.parse(flags)
                } catch (_: Arg2pException) {
                    return emptyMap()
                }
            val solver =
                try {
                    Arg2p.of(theory, parsed)
                } catch (_: Arg2pException) {
                    Arg2p.of("", parsed)
                }
            return solver.knownFlags.associateWith { flag ->
                try {
                    solver.setting(flag)
                } catch (_: Arg2pException) {
                    emptyList()
                }
            }
        }

        /** A graph of the API as plain data, read in [profile] */
        fun graphData(
            graph: Graph,
            profile: String,
        ): GraphData =
            GraphData(
                profile,
                graph.labellings.map { (argument, label) ->
                    ArgumentData(
                        id = argument.identifier,
                        descriptor = argument.descriptor,
                        conclusion = argument.conclusion,
                        topRule = argument.topRule,
                        label = label,
                        supports = argument.supports.map { it.identifier },
                    )
                },
                graph.attacks.map { AttackData(it.attacker.identifier, it.target.identifier, it.type, it.on?.identifier) },
            )

        private fun profileOf(solver: Arg2p): String =
            try {
                solver.setting("graphBuildMode").firstOrNull() ?: ""
            } catch (_: Arg2pException) {
                ""
            }

        private fun problemData(
            problem: SyntaxProblem,
            where: String?,
        ) = ProblemData(problem.line, problem.column, problem.length, problem.description, where)
    }
}
