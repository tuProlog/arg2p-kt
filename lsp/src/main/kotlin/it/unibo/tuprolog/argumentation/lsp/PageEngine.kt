package it.unibo.tuprolog.argumentation.lsp

import com.google.gson.JsonObject
import it.unibo.tuprolog.argumentation.Arg2pException
import it.unibo.tuprolog.argumentation.Flags
import java.util.concurrent.CancellationException
import java.util.concurrent.CompletableFuture
import java.util.concurrent.CompletionException
import java.util.concurrent.TimeUnit
import kotlin.time.Duration
import kotlin.time.Duration.Companion.minutes

/**
 * The engine of the pages of the solver, the components of @tuprolog/arg2p-ui, wherever a server shows them: the MCP
 * App of the MCP server (through its tool engine, see [McpApp]) and the panels of VS Code (through the request
 * arg2p/engine of the language server). A page speaks to it as the solver of the browser does to its worker (the
 * messages of @tuprolog/arg2p-ui/js, see host.ts there), each request answered with the messages the worker would
 * post:
 *
 * ```
 * { type: 'warmUp' }                                -> { type: 'warm' }
 * { type: 'solve', id, theory, query, flags,       -> { type: 'started', id, query } or { type: 'failed', id, error },
 *   parallel? }                                        after { type: 'output', id, text } if it printed
 * { type: 'next', id }                              -> { type: 'solution', id, solution, hasNext }, after its output
 * { type: 'graphAt', id, branch, reqId }            -> { type: 'graph', reqId, graph }
 * { type: 'check', reqId, text, kind }              -> { type: 'checked', reqId, problem }
 * { type: 'settings', reqId, theory, flags }        -> { type: 'settings', reqId, settings }
 * { type: 'stop' }                                  -> nothing: the request under way is stopped
 * { type: 'close' }                                 -> nothing: the page is forgotten
 * ```
 *
 * Each page is a session of its own, by the name it gives: its query (the last one alone, as in the browser), and the
 * requests solving it (solve and next, one at a time in their order, as the worker solves them), which a new query, a
 * stop or a close stops, answered with a failure. A session idle for [idle] is forgotten, the least recently used past
 * [MAX_SESSIONS] too: their query is no longer running. Each request is stopped past [limit], if there is one.
 *
 * A query solved with `parallel` is solved by the cluster of [federation] (on the threads of the server, and on the
 * sites it is federated with), on the same theory with the same flags, as by the solver of the page otherwise; with no
 * federation, by the solver of the page. The cluster keeps the graph of the last stage of a resolution alone: the
 * stages before it cannot be read back.
 */
internal class PageEngine(
    private val solving: Solving,
    private val idle: Duration = IDLE,
    private val limit: Duration = Duration.INFINITE,
    private val federation: Federation? = null,
) : AutoCloseable {
    // A query of a page, solved one solution at a time (one request at a time: a solver has no lock of its own),
    // what it prints kept until it is told
    private class Run(
        val id: Long,
        val resolution: Resolution,
        val printed: StringBuilder,
    ) {
        // What it printed since last asked
        fun output(): List<Answer> {
            val text = synchronized(printed) { printed.toString().also { printed.clear() } }
            return if (text.isEmpty()) emptyList() else listOf(answer("output", "id" to id, "text" to text))
        }
    }

    // A page: its query, the requests solving it, and when it last asked anything
    private class Session {
        var run: Run? = null
        val solving = mutableListOf<CompletableFuture<*>>()
        var used = System.nanoTime()

        // Stops the requests solving its query
        fun stop() {
            solving.toList().forEach { it.cancel(true) }
            solving.clear()
        }
    }

    // the sessions, the least recently used first
    private val sessions = LinkedHashMap<String, Session>(MAX_SESSIONS, 0.75f, true)

    // the idle sessions forgotten as time goes by, as well as at each request
    private val sweeping =
        Solving.timer.scheduleWithFixedDelay(
            ::expire,
            idle.inWholeMilliseconds,
            idle.inWholeMilliseconds,
            TimeUnit.MILLISECONDS,
        )

    /**
     * The answers to [request], a message of the page of [session] to its engine: stop and close are done at once,
     * the others solved on threads of their own (cancelling the result of one stops it). An [InvalidParams] for a
     * request that is not one of the engine.
     */
    fun handle(
        session: String,
        request: JsonObject,
    ): CompletableFuture<List<Answer>> {
        expire()
        val type = request.requiredText("type")
        if (type == "close") {
            forget(session)
            return done()
        }
        val page = session(session)
        return when (type) {
            "warmUp" -> done(answer("warm"))
            "solve" -> {
                val id = request.requiredInteger("id")
                val theory = request.text("theory") ?: ""
                val query = request.text("query") ?: ""
                val flags = request.text("flags") ?: ""
                val parallel = request.flag("parallel") ?: false
                solving(page, id, replacing = true) { start(page, id, theory, query, flags, parallel) }
            }
            "next" -> {
                val id = request.requiredInteger("id")
                solving(page, id, replacing = false) {
                    runOf(page, id)?.let(::next)
                        ?: listOf(failed(id, ErrorData("Error", NO_LONGER_RUNNING)))
                }
            }
            "graphAt" -> {
                val id = request.requiredInteger("id")
                val branch = request.requiredInteger("branch").toInt()
                val reqId = request.requiredInteger("reqId")
                submit { listOf(answer("graph", "reqId" to reqId, "graph" to graphAt(runOf(page, id), branch))) }
            }
            "check" -> {
                val reqId = request.requiredInteger("reqId")
                val text = request.text("text") ?: ""
                val kind = request.text("kind") ?: "theory"
                submit { listOf(answer("checked", "reqId" to reqId, "problem" to Solvers.check(text, kind))) }
            }
            "settings" -> {
                val reqId = request.requiredInteger("reqId")
                val theory = request.text("theory") ?: ""
                val flags = request.text("flags") ?: ""
                submit { listOf(answer("settings", "reqId" to reqId, "settings" to Solvers.settings(theory, flags))) }
            }
            "stop" -> {
                synchronized(sessions) { page.stop() }
                done()
            }
            else -> throw InvalidParams("Not a request of the engine: $type")
        }
    }

    /** How many pages there are */
    val pages: Int get() = synchronized(sessions) { sessions.size }

    /** Forgets every page, stopping what they solve */
    override fun close() {
        sweeping.cancel(false)
        synchronized(sessions) { sessions.keys.toList() }.forEach(::forget)
    }

    // The session of [name], made if there is none, the most recently used now; the least recently used forgotten past
    // MAX_SESSIONS
    private fun session(name: String): Session =
        synchronized(sessions) {
            val session = sessions.getOrPut(name) { Session() }.apply { used = System.nanoTime() }
            while (sessions.size > MAX_SESSIONS) sessions.remove(sessions.keys.first())?.stop()
            session
        }

    // The request of [page] solving its query, [work], after those before it ([replacing] them, it stops them):
    // answered with a failure if it is stopped or fails
    private fun solving(
        page: Session,
        id: Long,
        replacing: Boolean,
        work: () -> List<Answer>,
    ): CompletableFuture<List<Answer>> {
        val solved =
            synchronized(sessions) {
                if (replacing) page.stop()
                val before = page.solving.toList()
                submit {
                    before.forEach { request ->
                        try {
                            request.get()
                        } catch (_: InterruptedException) {
                            throw CancellationException(STOPPED)
                        } catch (_: Exception) {
                            // its failure is its own
                        }
                    }
                    work()
                }.also { page.solving += it }
            }
        // the server giving up the request (its client cancelled it) stops it too
        return solved.handling { answers, failure ->
            synchronized(sessions) { page.solving.remove(solved) }
            when (val cause = (failure as? CompletionException)?.cause ?: failure) {
                null -> answers.orEmpty()
                is CancellationException -> listOf(failed(id, ErrorData("Stopped", STOPPED)))
                else -> listOf(failed(id, ErrorData("Error", cause.message ?: cause.toString())))
            }
        }
    }

    // [work] solved on a thread of its own, within the limit
    private fun <T> submit(work: () -> T): CompletableFuture<T> =
        solving.submit { if (limit.isInfinite()) work() else Deadline.within(limit) { work() } }

    // Starts solving the query of [page], by the cluster of the federation if [parallel]: its run replaces the one before,
    // only if it is still the request of the page
    private fun start(
        page: Session,
        id: Long,
        theory: String,
        query: String,
        flags: String,
        parallel: Boolean,
    ): List<Answer> {
        synchronized(sessions) { page.run = null }
        if (query.isBlank()) return listOf(failed(id, ErrorData("Query error", "Query is a mandatory field.")))
        // the flags, the theory and the query are read in turn: the error tells which one it is about
        var reading = "flags"
        return try {
            val parsed = Flags.parse(flags)
            reading = "theory"
            val printed = StringBuilder()
            val print: (String) -> Unit = { text -> synchronized(printed) { printed.append(text) } }
            // the theory read by the solver of the page in either case: where it does not parse, and its profile
            val solver = Solvers.of(theory, parsed, print)
            // asked before the query: the setting does not change the context of the solver
            val profile = Solvers.profileOf(solver)
            reading = "query"
            val written = query.trim().removeSuffix(".")
            // the query read (where it does not parse), its solutions computed only as they are asked for
            val solutions = solver.solve(written)
            val resolution =
                federation?.takeIf { parallel }?.resolve(theory, parsed, written, profile, print)
                    ?: Resolution.Local(solver, profile, solutions)
            val run = Run(id, resolution, printed)
            synchronized(sessions) { if (!Thread.currentThread().isInterrupted) page.run = run }
            run.output() + answer("started", "id" to id, "query" to written)
        } catch (error: Arg2pException) {
            val name = "${reading.replaceFirstChar { it.uppercase() }} error"
            // what it printed goes with it
            listOf(failed(id, ErrorData(name, error.message ?: name, error.syntax?.let { ProblemData.of(it, reading) })))
        }
    }

    // The next solution of [run], with the graph of its stage, and whether there may be another one
    private fun next(run: Run): List<Answer> =
        synchronized(run) {
            val (solution, hasNext) =
                try {
                    run.resolution.next()
                } catch (error: Exception) {
                    SolutionData.halted(error) to false
                }
            run.output() + answer("solution", "id" to run.id, "solution" to solution, "hasNext" to hasNext)
        }

    // The graph of the stage [branch] of [run]: none for a stage that cannot be read, or a query no longer running
    private fun graphAt(
        run: Run?,
        branch: Int,
    ): GraphData = run?.let { synchronized(it) { it.resolution.graphAt(branch) } } ?: GraphData.EMPTY

    // The run of [page], if it is the query [id]
    private fun runOf(
        page: Session,
        id: Long,
    ): Run? = synchronized(sessions) { page.run?.takeIf { it.id == id } }

    private fun forget(name: String) = synchronized(sessions) { sessions.remove(name)?.stop() }

    // Forgets the sessions idle for too long
    private fun expire() {
        val now = System.nanoTime()
        synchronized(sessions) { sessions.filterValues { now - it.used > idle.inWholeNanoseconds }.keys.toList() }.forEach(::forget)
    }

    companion object {
        /** How many pages are kept at most */
        const val MAX_SESSIONS = 32

        /** How long a page is kept without asking anything */
        val IDLE = 30.minutes

        /** What a request of a query no longer running fails with */
        const val NO_LONGER_RUNNING = "This query is no longer running."

        /** What a request that was stopped fails with */
        const val STOPPED = "The query was stopped."

        private fun answer(
            type: String,
            vararg fields: Pair<String, Any?>,
        ): Answer = mapOf("type" to type, *fields)

        /** The answer of the request [id] of a query that failed for [error] */
        internal fun failed(
            id: Long,
            error: ErrorData,
        ) = answer("failed", "id" to id, "error" to error)

        private fun done(vararg answers: Answer) = CompletableFuture.completedFuture(answers.toList())
    }
}
