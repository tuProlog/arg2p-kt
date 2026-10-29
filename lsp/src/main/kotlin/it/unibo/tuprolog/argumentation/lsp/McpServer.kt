package it.unibo.tuprolog.argumentation.lsp

import com.google.gson.JsonArray
import com.google.gson.JsonElement
import com.google.gson.JsonNull
import com.google.gson.JsonObject
import com.google.gson.JsonParseException
import com.google.gson.JsonParser
import com.google.gson.JsonPrimitive
import it.unibo.tuprolog.argumentation.Arg2p
import it.unibo.tuprolog.argumentation.Flags
import it.unibo.tuprolog.argumentation.Solution
import java.io.BufferedReader
import java.io.InputStream
import java.io.InputStreamReader
import java.io.OutputStream
import java.io.PrintStream
import java.util.concurrent.CompletableFuture
import java.util.concurrent.CompletionException
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.TimeUnit
import kotlin.time.Duration.Companion.seconds

/**
 * The MCP server of arg2p: the tools an AI assistant solves theories with (solve, answer, check, settings), over the
 * standard input and output, one JSON-RPC message a line (the transport stdio of the Model Context Protocol).
 *
 * The tools are solved on threads of their own while the server goes on reading: a ping is answered meanwhile, and
 * notifications/cancelled stops the request it names. A tool that fails (a theory that does not parse, a profile
 * refusing it, arguments it does not take, a query taking longer than its timeout, which stops it) answers with its
 * error, as MCP tools do, rather than failing the request; a message that is not a request of the server, or not well
 * formed, is answered with the error of JSON-RPC.
 *
 * The theories are written by an AI model: they are solved within their timeout.
 *
 * The tool explore shows the solver of a theory to the user: as an MCP App (see [McpApp]) where the client declares it
 * shows them, and as a link to the web playground everywhere.
 */
internal class McpServer(
    input: InputStream,
    output: OutputStream,
) {
    // What a tool found, and the text telling it (its JSON otherwise)
    private class Found(
        val content: Map<String, Any?>,
        val text: String = json.toJson(content),
    )

    // A tool that ran out of time
    private class TimedOut(
        seconds: Long,
    ) : Exception("No answer within $seconds seconds: the theory may not terminate, or need a larger timeoutSeconds.")

    private val reader = BufferedReader(InputStreamReader(input, Charsets.UTF_8))
    private val writer = PrintStream(output, true, Charsets.UTF_8)
    private val solving = Solving("arg2p-mcp")
    private val pages = PageEngine(solving, limit = DEFAULT_TIMEOUT.seconds)

    // the tools under way, by the id of their request: notifications/cancelled stops them
    private val requests = ConcurrentHashMap<JsonElement, CompletableFuture<*>>()

    // their answers, until written
    private val answering = ConcurrentHashMap.newKeySet<CompletableFuture<*>>()

    // whether the client shows MCP Apps, as it declares in initialize
    @Volatile
    private var apps = false

    /** How many tools are being solved */
    val busy: Int get() = solving.busy

    /** Answers the messages of the client until it closes the input */
    fun run() {
        while (true) {
            val line = reader.readLine() ?: break
            if (line.isBlank()) continue
            val message =
                try {
                    JsonParser.parseString(line)
                } catch (_: JsonParseException) {
                    respond(JsonNull.INSTANCE, error = error(PARSE_ERROR, "Parse error"))
                    continue
                }
            receive(message)
        }
        // the client is gone: what it asked is answered, within its timeout
        answering.toList().forEach { runCatching { it.get(MAX_TIMEOUT + 1, TimeUnit.SECONDS) } }
        pages.close()
        solving.close()
    }

    // A message of the client: a request, answered, or a notification (a response, the server asking nothing, is not
    // one it waits for)
    private fun receive(message: JsonElement) {
        val request = message as? JsonObject ?: return respond(JsonNull.INSTANCE, error = error(INVALID_REQUEST, "Not a JSON-RPC message"))
        val id = request.get("id")?.takeUnless { it.isJsonNull }
        if (!request.has("method")) return
        val method =
            (request.get("method") as? JsonPrimitive)?.takeIf { it.isString }?.asString
                ?: return respond(id ?: JsonNull.INSTANCE, error = error(INVALID_REQUEST, "The method is not a text"))
        try {
            val params = request.obj("params") ?: JsonObject()
            if (id == null) notified(method, params) else answer(id, method, params)
        } catch (failure: InvalidParams) {
            if (id != null) respond(id, error = error(INVALID_PARAMS, failure.message ?: "Invalid params"))
        } catch (failure: Exception) {
            if (id != null) respond(id, error = error(INTERNAL_ERROR, failure.message ?: failure.toString()))
        }
    }

    private fun notified(
        method: String,
        params: JsonObject,
    ) {
        // the client gave up the request: its work stops, unanswered
        if (method == "notifications/cancelled") params.get("requestId")?.let { requests[it]?.cancel(true) }
    }

    private fun answer(
        id: JsonElement,
        method: String,
        params: JsonObject,
    ) = when (method) {
        "initialize" -> respond(id, initialize(params))
        "ping" -> respond(id, JsonObject())
        "tools/list" -> respond(id, JsonObject().apply { add("tools", json.toJsonTree(tools())) })
        "tools/call" -> call(id, params)
        "resources/list" -> respond(id, JsonObject().apply { add("resources", json.toJsonTree(listOf(RESOURCE))) })
        "resources/templates/list" -> respond(id, JsonObject().apply { add("resourceTemplates", JsonArray()) })
        "resources/read" -> {
            val uri = params.requiredText("uri")
            if (uri == McpApp.URI) {
                respond(id, JsonObject().apply { add("contents", json.toJsonTree(listOf(RESOURCE + ("text" to McpApp.page)))) })
            } else {
                respond(id, error = error(RESOURCE_NOT_FOUND, "Resource not found: $uri"))
            }
        }
        else -> respond(id, error = error(METHOD_NOT_FOUND, "Method not found: $method"))
    }

    private fun initialize(params: JsonObject): JsonObject {
        val asked = params.get("protocolVersion")?.takeIf { it.isJsonPrimitive }?.asString
        apps = (params.get("capabilities") as? JsonObject)?.let { (it.get("extensions") as? JsonObject)?.has(McpApp.EXTENSION) } == true
        return JsonObject().apply {
            addProperty("protocolVersion", if (asked in PROTOCOL_VERSIONS) asked else PROTOCOL_VERSIONS.first())
            add(
                "capabilities",
                JsonObject().apply {
                    add("tools", JsonObject())
                    add("resources", JsonObject())
                    add("extensions", JsonObject().apply { add(McpApp.EXTENSION, JsonObject()) })
                },
            )
            add(
                "serverInfo",
                JsonObject().apply {
                    addProperty("name", "arg2p")
                    addProperty("version", Arg2p.version())
                },
            )
            addProperty("instructions", INSTRUCTIONS)
        }
    }

    // A tool called: its arguments read at once, it is solved on a thread of its own and answered from there, as MCP
    // gives the result of a tool (its JSON as text and as structured content), or its error
    private fun call(
        id: JsonElement,
        params: JsonObject,
    ) {
        val name = params.requiredText("name")
        val arguments = params.obj("arguments") ?: JsonObject()
        val work =
            try {
                if (name == "engine") {
                    val session = arguments.requiredText("session")
                    val request = arguments.obj("request") ?: throw InvalidParams("request is missing")
                    pages.handle(session, request).handling { answers, failure -> Found(mapOf("answers" to (answers ?: throw failure!!))) }
                } else {
                    val tool = toolOf(name, arguments)
                    val timeout = (arguments.integer("timeoutSeconds") ?: DEFAULT_TIMEOUT).coerceIn(1, MAX_TIMEOUT)
                    solving.submit { timed(timeout, tool) }
                }
            } catch (failure: InvalidParams) {
                if (name !in TOOL_NAMES) throw InvalidParams("Unknown tool: $name")
                return respond(id, failure("Invalid arguments of $name: ${failure.message}"))
            }
        requests[id] = work
        val answered =
            work.handle { found, failure ->
                // a request the client cancelled is not answered
                if (!work.isCancelled) {
                    val cause = (failure as? CompletionException)?.cause ?: failure
                    respond(id, if (cause == null) result(found!!) else failure(cause.message ?: cause.toString()))
                }
                requests.remove(id, work)
            }
        answering += answered
        answered.whenComplete { _, _ -> answering -= answered }
    }

    // [tool] within [timeout] seconds: an error if it runs out of time, unless it answers with what it found so far
    private fun timed(
        timeout: Long,
        tool: (Deadline) -> Found,
    ): Found =
        Deadline.within(timeout.seconds) { deadline ->
            try {
                tool(deadline)
            } catch (failure: Exception) {
                throw if (deadline.expired) TimedOut(timeout) else failure
            }
        }

    // The work of the tool [name] on [arguments], read at once: an InvalidParams if they are not those it takes
    private fun toolOf(
        name: String,
        arguments: JsonObject,
    ): (Deadline) -> Found =
        when (name) {
            "solve" -> {
                val theory = arguments.requiredText("theory")
                val query = arguments.text("query") ?: Solvers.DEFAULT_QUERY
                val flags = flagsOf(arguments)
                val most = (arguments.integer("maxSolutions") ?: DEFAULT_SOLUTIONS).coerceIn(1, MAX_SOLUTIONS).toInt()
                tool { deadline -> solve(theory, query, flags, most, deadline) }
            }
            "answer" -> {
                val theory = arguments.requiredText("theory")
                val query = arguments.requiredText("query")
                val flags = flagsOf(arguments)
                tool { answer(theory, query, flags) }
            }
            "check" -> {
                val text = arguments.requiredText("text")
                val kind = arguments.text("kind") ?: "theory"
                if (kind !in KINDS) throw InvalidParams("kind is not one of ${KINDS.joinToString()}")
                tool {
                    val problem = Solvers.check(text, kind)
                    Found(mapOf("parses" to (problem == null), "problem" to problem))
                }
            }
            "settings" -> {
                val theory = arguments.requiredText("theory")
                val flags = flagsOf(arguments)
                tool {
                    parsing(theory)
                    Flags.parse(flags)
                    Found(Solvers.settings(theory, flags))
                }
            }
            "explore" -> {
                val theory = arguments.requiredText("theory")
                val query = arguments.text("query") ?: Solvers.DEFAULT_QUERY
                val flags = flagsOf(arguments)
                tool { explore(theory, query, flags) }
            }
            else -> throw InvalidParams("Unknown tool: $name")
        }

    // The work of a tool, as it is written
    private fun tool(work: (Deadline) -> Found) = work

    // The first solutions of [query], those found in time if it runs out of it, and the graph of the last yes
    private fun solve(
        theory: String,
        query: String,
        flags: String,
        most: Int,
        deadline: Deadline,
    ): Found {
        val solver = Solvers.of(theory, Flags.parse(flags))
        val profile = Solvers.profileOf(solver)
        val solutions = solver.solve(query)
        val found = mutableListOf<Solution>()
        var graph = GraphData.EMPTY
        // the solutions asked for, no more: whether there may be others is known without looking for them
        while (found.size < most && solutions.hasNext()) {
            val solution = solutions.next()
            // out of time, the resolution halts
            if (solution.isHalt && deadline.expired) break
            found += solution
            if (solution.isYes) {
                graph =
                    try {
                        GraphData.of(solver.graph(), profile)
                    } catch (failure: Exception) {
                        if (deadline.expired) break else throw failure
                    }
            }
        }
        // nothing found in time: an error (see timed)
        if (deadline.expired && found.isEmpty()) error("No solution in time")
        val content =
            mapOf(
                "profile" to profile,
                "solutions" to
                    found.map { mapOf("result" to it.result, "query" to it.query, "bindings" to BindingData.of(it), "error" to it.error) },
                "more" to (deadline.expired || solutions.hasNext()),
                "timedOut" to deadline.expired,
                "graph" to graph,
            )
        return if (deadline.expired) {
            Found(content, "Out of time: the solutions found until then. " + json.toJson(content))
        } else {
            Found(content)
        }
    }

    private fun answer(
        theory: String,
        query: String,
        flags: String,
    ): Found {
        val answer = Solvers.of(theory, Flags.parse(flags)).answer(query)
        return Found(
            mapOf(
                "in" to answer.inStatements.map { it.toString() },
                "out" to answer.outStatements.map { it.toString() },
                "und" to answer.undStatements.map { it.toString() },
            ),
        )
    }

    // What the solver is shown with: the theory, if it parses, its query, its flags, and the link to the playground
    private fun explore(
        theory: String,
        query: String,
        flags: String,
    ): Found {
        parsing(theory)
        val playground = Playground.link(theory, query, flags)
        val content =
            mapOf(
                "theory" to theory,
                "query" to query,
                "flags" to flags,
                "profile" to Solvers.profileOf(Solvers.of(theory, Flags.parse(flags))),
                "playground" to playground,
            )
        val text =
            if (apps) {
                "The solver of the theory is shown to the user in the conversation: they can run the query ($query), " +
                    "browse its solutions, their argument graphs and stages, and change the flags. The same theory opens " +
                    "in the web playground: $playground"
            } else {
                "The theory opens in the web playground, for the user to run the query ($query), browse its solutions, " +
                    "their argument graphs and stages, and change the flags: $playground"
            }
        return Found(content, text)
    }

    // [theory], refused if it does not parse
    private fun parsing(theory: String) {
        Solvers.check(theory, "theory")?.let {
            throw InvalidParams("The theory does not parse at line ${it.line}, column ${it.column}: ${it.message}")
        }
    }

    private fun flagsOf(arguments: JsonObject): String = arguments.text("flags") ?: Solvers.DEFAULT_FLAGS

    // The tools, as the client is shown them: the engine of the page and the page of explore only to a client of MCP Apps
    private fun tools(): List<Map<String, Any>> =
        if (apps) {
            TOOLS.map { if (it["name"] == "explore") it + ("_meta" to EXPLORE_META) else it } + ENGINE
        } else {
            TOOLS
        }

    private fun result(found: Found): JsonObject =
        JsonObject().apply {
            add("content", content(found.text))
            add("structuredContent", json.toJsonTree(found.content))
            addProperty("isError", false)
        }

    private fun failure(message: String): JsonObject =
        JsonObject().apply {
            add("content", content(message))
            addProperty("isError", true)
        }

    private fun content(text: String): JsonArray =
        JsonArray().apply {
            add(
                JsonObject().apply {
                    addProperty("type", "text")
                    addProperty("text", text)
                },
            )
        }

    private fun error(
        code: Int,
        message: String,
    ): JsonObject =
        JsonObject().apply {
            addProperty("code", code)
            addProperty("message", message)
        }

    @Synchronized
    private fun respond(
        id: JsonElement,
        result: JsonElement? = null,
        error: JsonObject? = null,
    ) {
        val response =
            JsonObject().apply {
                addProperty("jsonrpc", "2.0")
                add("id", id)
                if (error != null) add("error", error) else add("result", result ?: JsonObject())
            }
        writer.println(json.toJson(response))
    }

    companion object {
        /** The versions of the protocol the server speaks, the latest first */
        val PROTOCOL_VERSIONS = listOf("2025-06-18", "2025-03-26", "2024-11-05")

        const val DEFAULT_SOLUTIONS = 5L
        const val MAX_SOLUTIONS = 100L

        /** How long a tool runs by default, and at most, in seconds */
        const val DEFAULT_TIMEOUT = 60L
        const val MAX_TIMEOUT = 600L

        // The errors of JSON-RPC, and of MCP
        private const val PARSE_ERROR = -32700
        private const val INVALID_REQUEST = -32600
        private const val METHOD_NOT_FOUND = -32601
        private const val INVALID_PARAMS = -32602
        private const val INTERNAL_ERROR = -32603
        private const val RESOURCE_NOT_FOUND = -32002

        // What a text to check is
        private val KINDS = listOf("theory", "query")

        private const val THEORY =
            "The theory, in the language of arg2p: rules `r1 : bird(X) => flies(X).` (defeasible) and `r2 : a -> b.` " +
                "(strict), premises `f1 :=> bird(tweety).`, axioms `f2 :-> a.`, preferences `sup(r1, r2).`, negation " +
                "`-flies(X)`, Prolog clauses. Its profile, `graphBuildMode(aspic_plus).` (the default), `aspic`, " +
                "`aspic_defeasible`, `meta`, `aba` (assumption/1, contrary/2, strict rules), `aba_plus`, `abstract` " +
                "(arguments `a.`, attacks `a -> b.`, `a <-> b.`) or `free`, tells the framework it is read in."
        private const val FLAGS =
            "The flags, as Prolog facts: argumentLabellingMode(grounded|complete|preferred|stable|semistable|ideal|" +
                "eager|naive|stage|cf2|stage2|...), statementLabellingMode(statement), queryMode, graphBuildMode(...), " +
                "... Default: grounded semantics, standard statement labelling, query mode."
        private const val TIMEOUT = "How long to wait, in seconds (default $DEFAULT_TIMEOUT, at most $MAX_TIMEOUT)"

        private val INSTRUCTIONS =
            "Arg2P solves structured argumentation theories (ASPIC+, ABA, abstract frameworks) in Prolog. Use `check` to " +
                "see whether a theory parses, `answer` for the statements a query labels in, out and und, `solve` for " +
                "the solutions of any Prolog query on the theory with the argument graph (arguments, labels, attacks), " +
                "and `settings` for the profile and the flags in effect; `explore` shows the user the solver of a theory, " +
                "to browse its results themselves. A theory refused by its profile, or that does " +
                "not parse, gives an error saying why: do not guess the answer instead. Documentation: " +
                "https://tuprolog.github.io/arg2p-kt/"

        private fun schema(
            vararg properties: Pair<String, Map<String, Any>>,
            required: List<String>,
        ) = mapOf("type" to "object", "properties" to properties.toMap(), "required" to required)

        private fun text(description: String) = mapOf("type" to "string", "description" to description)

        private fun integer(description: String) = mapOf("type" to "integer", "description" to description)

        private val TOOLS: List<Map<String, Any>> =
            listOf(
                mapOf(
                    "name" to "solve",
                    "title" to "Solve a query",
                    "description" to
                        "Solves a query on a theory of arg2p: its first solutions (yes, no, or halt with an error), with " +
                        "their bindings, and the argument graph of the last yes (every argument with its conclusion, top " +
                        "rule, sub-arguments and label in, out or und; every attack with its type); more tells whether the " +
                        "search has alternatives left (they may give other solutions, or no). The default query, " +
                        "buildLabelSets(In, Out, Und), gives the statements labelled in, out and und. Out of time, it gives " +
                        "the solutions found until then (timedOut).",
                    "inputSchema" to
                        schema(
                            "theory" to text(THEORY),
                            "query" to text("A Prolog goal on the theory (default: ${Solvers.DEFAULT_QUERY})"),
                            "flags" to text(FLAGS),
                            "maxSolutions" to integer("How many solutions at most (default $DEFAULT_SOLUTIONS, at most $MAX_SOLUTIONS)"),
                            "timeoutSeconds" to integer(TIMEOUT),
                            required = listOf("theory"),
                        ),
                ),
                mapOf(
                    "name" to "answer",
                    "title" to "Label the statements of a query",
                    "description" to
                        "The statements matching a query (a literal, as flies(X) or -flies(tweety)) labelled in " +
                        "(justified), out (rejected) and und (undecided) by the theory, under its semantics.",
                    "inputSchema" to
                        schema(
                            "theory" to text(THEORY),
                            "query" to text("A literal, as flies(tweety) or flies(X)"),
                            "flags" to text(FLAGS),
                            "timeoutSeconds" to integer(TIMEOUT),
                            required = listOf("theory", "query"),
                        ),
                ),
                mapOf(
                    "name" to "check",
                    "title" to "Check a theory or a query",
                    "description" to "Whether a theory (or flags) or a query parses, and where it does not (line, column, token).",
                    "inputSchema" to
                        schema(
                            "text" to text("The text"),
                            "kind" to
                                mapOf(
                                    "type" to "string",
                                    "enum" to KINDS,
                                    "description" to "What the text is (default theory)",
                                ),
                            required = listOf("text"),
                        ),
                ),
                mapOf(
                    "name" to "settings",
                    "title" to "The settings in effect",
                    "description" to
                        "The value in effect of every flag for a theory: its profile (graphBuildMode), the semantics, the " +
                        "graph extensions, ... as the theory, the flags and the profile set them. A theory that does not " +
                        "parse is refused.",
                    "inputSchema" to
                        schema(
                            "theory" to text(THEORY),
                            "flags" to text(FLAGS),
                            "timeoutSeconds" to integer(TIMEOUT),
                            required = listOf("theory"),
                        ),
                ),
                mapOf(
                    "name" to "explore",
                    "title" to "Show the solver of a theory",
                    "description" to
                        "Shows the user the solver of a theory, to explore its results themselves: the query run, its " +
                        "solutions, the argument graph of each and the stages of its computation, the drawing of an " +
                        "abstract framework, the flags. Hosts of MCP Apps show it in the conversation; the result links the " +
                        "same theory in the web playground, for every host. Use it when the user wants to see or explore " +
                        "the results.",
                    "inputSchema" to
                        schema(
                            "theory" to text(THEORY),
                            "query" to text("The query run first (default: ${Solvers.DEFAULT_QUERY})"),
                            "flags" to text(FLAGS),
                            "timeoutSeconds" to integer(TIMEOUT),
                            required = listOf("theory"),
                        ),
                ),
            )

        // The names of the tools, the engine of the page too
        private val TOOL_NAMES = TOOLS.map { it["name"] } + "engine"

        // The page of explore, for the hosts of MCP Apps
        private val EXPLORE_META =
            mapOf(
                "ui" to mapOf("resourceUri" to McpApp.URI, "visibility" to listOf("model", "app")),
                // the key of the first hosts of MCP Apps
                "ui/resourceUri" to McpApp.URI,
            )

        // The engine of the page of the solver, which only the page calls
        private val ENGINE: Map<String, Any> =
            mapOf(
                "name" to "engine",
                "title" to "The engine of the solver",
                "description" to "The engine the solver shown by explore runs on: for that solver only, not to be called otherwise.",
                "inputSchema" to
                    mapOf(
                        "type" to "object",
                        "properties" to
                            mapOf(
                                "session" to text("The solver asking"),
                                "request" to mapOf("type" to "object", "description" to "A message of the solver to its engine"),
                            ),
                        "required" to listOf("session", "request"),
                    ),
                "_meta" to mapOf("ui" to mapOf("visibility" to listOf("app"))),
            )

        /** The page of the solver, as a resource (its text added when read) */
        private val RESOURCE: Map<String, Any> =
            mapOf(
                "uri" to McpApp.URI,
                "name" to "solver",
                "title" to "The solver of Arg2P",
                "description" to "The solver of a theory: its query, solutions, argument graphs and flags",
                "mimeType" to McpApp.MIME_TYPE,
                "_meta" to McpApp.meta,
            )
    }
}
