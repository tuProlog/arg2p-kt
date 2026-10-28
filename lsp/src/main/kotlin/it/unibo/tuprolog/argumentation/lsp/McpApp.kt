package it.unibo.tuprolog.argumentation.lsp

import com.google.gson.JsonObject

/**
 * The solver pane of the MCP server, an MCP App (the extension io.modelcontextprotocol/ui of MCP): the page of the
 * solver, served as the resource [URI], which the hosts of MCP Apps show when the tool explore is called, and the engine
 * that page solves on, through the tool engine, which only the page calls.
 *
 * The page speaks to its engine as the solver of the browser does to its worker (the messages of
 * `@tuprolog/arg2p-ui/js`): a request, and the answers to it. Each page is a session of its own, with its runs: the
 * ids the pages give their runs repeat across pages.
 */
class McpApp {
    // what the runs print, until the page asks for their next solution
    private val outputs = mutableMapOf<Int, StringBuilder>()
    private val engine =
        Engine { id, text ->
            synchronized(outputs) { outputs.getOrPut(id) { StringBuilder() }.append(text) }
        }

    // the runs of the engine, by the session of their page and the id the page gives them
    private val runs = mutableMapOf<Pair<String, Int>, Int>()

    /** The answers to [request], a message of the page of [session] to its engine */
    fun handle(
        session: String,
        request: JsonObject,
    ): List<Map<String, Any?>> {
        fun text(name: String) = request.get(name)?.takeUnless { it.isJsonNull }?.asString ?: ""

        fun number(name: String) = request.get(name)?.asInt ?: 0
        val id = number("id")
        return when (val type = text("type")) {
            "warmUp" -> listOf(mapOf("type" to "warm"))
            "solve" -> {
                // the page shows its last query alone: the engine forgets the others
                close(session)
                val answer = engine.solve(text("theory"), text("query"), text("flags"))
                if (answer.error != null) {
                    listOf(mapOf("type" to "failed", "id" to id, "error" to answer.error))
                } else {
                    synchronized(runs) { runs[session to id] = answer.id }
                    listOf(mapOf("type" to "started", "id" to id, "query" to answer.query)) + printed(answer.id, id)
                }
            }
            "next" -> {
                val run = synchronized(runs) { runs[session to id] } ?: return listOf(failed(id, "This query is no longer running."))
                val answer = engine.next(run)
                printed(run, id) +
                    if (answer.solution == null) {
                        failed(id, answer.error?.message ?: "No solution")
                    } else {
                        mapOf("type" to "solution", "id" to id, "solution" to answer.solution, "hasNext" to answer.hasNext)
                    }
            }
            "graphAt" -> {
                val run = synchronized(runs) { runs[session to id] }
                val graph = if (run == null) GraphData.EMPTY else engine.graphAt(run, number("branch"))
                listOf(mapOf("type" to "graph", "reqId" to number("reqId"), "graph" to graph))
            }
            "check" -> listOf(mapOf("type" to "checked", "reqId" to number("reqId"), "problem" to Engine.check(text("text"), text("kind"))))
            "settings" ->
                listOf(
                    mapOf(
                        "type" to "settings",
                        "reqId" to number("reqId"),
                        "settings" to Engine.settings(text("theory"), text("flags")),
                    ),
                )
            // the page stopped, or closed: its runs are forgotten
            "close" -> {
                close(session)
                emptyList()
            }
            else -> throw IllegalArgumentException("Unknown request of the engine: $type")
        }
    }

    // What the run [run] printed since last asked, as the outputs of the run [id] of the page
    private fun printed(
        run: Int,
        id: Int,
    ): List<Map<String, Any?>> {
        val text = synchronized(outputs) { outputs.remove(run)?.toString() } ?: return emptyList()
        return listOf(mapOf("type" to "output", "id" to id, "text" to text))
    }

    private fun failed(
        id: Int,
        message: String,
    ) = mapOf("type" to "failed", "id" to id, "error" to ErrorData("Error", message))

    private fun close(session: String) {
        val closed = synchronized(runs) { runs.keys.filter { it.first == session }.mapNotNull { runs.remove(it) } }
        closed.forEach {
            engine.close(it)
            synchronized(outputs) { outputs.remove(it) }
        }
    }

    companion object {
        /** The resource of the page of the solver */
        const val URI = "ui://arg2p/solver"

        /** The type of the resources of MCP Apps */
        const val MIME_TYPE = "text/html;profile=mcp-app"

        /** The extension of MCP the apps are */
        const val EXTENSION = "io.modelcontextprotocol/ui"

        /** The version of @tuprolog/arg2p-ui the page loads (ARG2P_UI_VERSION another one, to try it) */
        val UI_VERSION: String = System.getenv("ARG2P_UI_VERSION")?.takeIf { it.isNotBlank() } ?: "0.2.0"

        /** Where the page loads its components and their dependencies from */
        const val CDN = "https://esm.sh"

        /** The page of the solver */
        val page: String by lazy {
            val text =
                McpApp::class.java
                    .getResourceAsStream("solver.html")
                    ?.use { it.readBytes().toString(Charsets.UTF_8) }
                    ?: error("The page of the solver is missing from the jar")
            text.replace("{{ui}}", UI_VERSION).replace("{{cdn}}", CDN)
        }

        /** The metadata of the page for the hosts: what it loads, from where */
        val meta: Map<String, Any> =
            mapOf(
                "ui" to
                    mapOf(
                        "csp" to mapOf("resourceDomains" to listOf(CDN), "connectDomains" to emptyList<String>()),
                        "prefersBorder" to true,
                    ),
            )
    }
}
