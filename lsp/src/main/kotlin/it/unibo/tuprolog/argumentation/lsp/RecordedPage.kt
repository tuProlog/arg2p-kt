package it.unibo.tuprolog.argumentation.lsp

import java.util.concurrent.TimeUnit
import java.util.concurrent.TimeoutException
import kotlin.time.Duration
import kotlin.time.Duration.Companion.seconds

/**
 * The recorded page of the solver: a run of a query already solved, shown by the solver of @tuprolog/arg2p-ui in a page
 * of its own that solves nothing, for where no engine can run beside the page (the sandboxes of AI assistants, whose
 * pages start no workers and reach no server: claude.ai, say). Python makes it (arg2p page, through JPype).
 *
 * The run is recorded by the engine of the pages (see [PageEngine]), as the components read it: the answers to solve
 * (the query started, or failed), those to next until there is no other solution or there are as many as asked for,
 * and the settings in effect; within a time limit, past which it stops, as the tools of the MCP server do. The page
 * replays them, and them alone: another theory, query or flags is solved by whoever made the page, or in the web
 * playground the page links.
 */
object RecordedPage {
    /** The query solved when none is given: the statements labelled in, out and und */
    const val DEFAULT_QUERY = Solvers.DEFAULT_QUERY

    /** The flags a theory is solved with when none are given (see [Solvers.DEFAULT_FLAGS]) */
    @JvmField
    val DEFAULT_FLAGS: String = Solvers.DEFAULT_FLAGS

    /** How many solutions are recorded by default */
    const val DEFAULT_SOLUTIONS = 20

    /** How long the run is recorded by default, in seconds: as long as a tool of the MCP server runs by default */
    const val DEFAULT_TIMEOUT = McpServer.DEFAULT_TIMEOUT.toInt()

    // The page the run is recorded as, and its query
    private const val SESSION = "recorded"
    private const val ID = 1L

    /**
     * The page of [query] solved on [theory] with [flags] (Prolog facts): its first [maxSolutions] solutions with their
     * graphs, and the settings in effect, those recorded within [timeoutSeconds]
     */
    @JvmStatic
    @JvmOverloads
    fun page(
        theory: String,
        query: String = DEFAULT_QUERY,
        flags: String = DEFAULT_FLAGS,
        maxSolutions: Int = DEFAULT_SOLUTIONS,
        timeoutSeconds: Int = DEFAULT_TIMEOUT,
    ): String {
        val recording = record(theory, query, flags, maxSolutions.coerceAtLeast(1), timeoutSeconds.coerceAtLeast(1).seconds)
        return Pages.render(
            "recorded.html",
            mapOf(
                "ui" to Pages.UI_VERSION,
                "title" to escaped("Arg2P · $query"),
                "playground" to escaped(Playground.link(theory, query, flags)),
                "recording" to Pages.script(recording),
            ),
        )
    }

    /**
     * A run recorded: what it solved, the answers of the engine of the pages to solve and to each next, the settings
     * in effect, whether there may be more solutions than those recorded, and whether it ran out of [timeout] seconds
     */
    internal class Recording(
        val theory: String,
        val query: String,
        val flags: String,
        val solve: List<Answer>,
        val next: List<List<Answer>>,
        val settings: Any?,
        val more: Boolean,
        val timedOut: Boolean,
        val timeout: Long,
    )

    /** The run of [query] on [theory] with [flags], its first [most] solutions, recorded within [timeout] */
    internal fun record(
        theory: String,
        query: String,
        flags: String,
        most: Int,
        timeout: Duration,
    ): Recording =
        Solving("arg2p-page").use { solving ->
            PageEngine(solving).use { engine ->
                val end = System.nanoTime() + timeout.inWholeNanoseconds
                var timedOut = false

                // The answers to [request], none if the time runs out first, which stops it
                fun ask(vararg request: Pair<String, Any>): List<Answer>? {
                    val asked = engine.handle(SESSION, json.toJsonTree(mapOf(*request)).asJsonObject)
                    return try {
                        asked.get(end - System.nanoTime(), TimeUnit.NANOSECONDS)
                    } catch (_: TimeoutException) {
                        asked.cancel(true)
                        timedOut = true
                        null
                    }
                }

                val settings = ask("type" to "settings", "reqId" to ID, "theory" to theory, "flags" to flags)
                val solve =
                    ask("type" to "solve", "id" to ID, "theory" to theory, "query" to query, "flags" to flags)
                        ?: listOf(PageEngine.failed(ID, ErrorData("Error", TimedOut(timeout).message!!)))
                val next = mutableListOf<List<Answer>>()
                var more = solve.any { it["type"] == "started" }
                while (more && next.size < most) {
                    val answers = ask("type" to "next", "id" to ID) ?: break
                    next += answers
                    more = answers.any { it["type"] == "solution" && it["hasNext"] == true }
                }
                Recording(
                    theory,
                    query,
                    flags,
                    solve,
                    next,
                    settings?.firstOrNull { it["type"] == "settings" }?.get("settings") ?: emptyMap<String, Any>(),
                    more,
                    timedOut,
                    timeout.inWholeSeconds,
                )
            }
        }

    // [text] within the text or an attribute of the page
    private fun escaped(text: String): String =
        text
            .replace("&", "&amp;")
            .replace("<", "&lt;")
            .replace(">", "&gt;")
            .replace("\"", "&quot;")
            .replace("'", "&#39;")
}
