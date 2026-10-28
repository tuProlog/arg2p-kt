package it.unibo.tuprolog.argumentation.lsp

import com.google.gson.Gson
import com.google.gson.GsonBuilder
import com.google.gson.JsonArray
import com.google.gson.JsonElement
import com.google.gson.JsonNull
import com.google.gson.JsonObject
import com.google.gson.JsonParser
import it.unibo.tuprolog.argumentation.Arg2p
import it.unibo.tuprolog.argumentation.Arg2pException
import it.unibo.tuprolog.argumentation.Flags
import java.io.BufferedReader
import java.io.InputStream
import java.io.InputStreamReader
import java.io.OutputStream
import java.io.PrintStream
import java.util.concurrent.ExecutionException
import java.util.concurrent.Executors
import java.util.concurrent.TimeUnit
import java.util.concurrent.TimeoutException

/**
 * The MCP server of arg2p: the tools an AI assistant solves theories with (solve, answer, check, settings), over the
 * standard input and output, one JSON-RPC message a line (the transport stdio of the Model Context Protocol).
 *
 * A tool that fails (a theory that does not parse, a profile refusing it, a query taking longer than its timeout)
 * answers with its error, as MCP tools do, rather than failing the request.
 *
 * The tool explore shows the solver of a theory to the user: as an MCP App (see [McpApp]) where the host shows them,
 * and as a link to the web playground everywhere.
 */
class McpServer(
    input: InputStream,
    output: OutputStream,
) {
    private val reader = BufferedReader(InputStreamReader(input, Charsets.UTF_8))
    private val writer = PrintStream(output, true, Charsets.UTF_8)
    private val gson: Gson = GsonBuilder().serializeNulls().create()
    private val app = McpApp()
    private val solving =
        Executors.newCachedThreadPool {
            Thread(null, it, "arg2p-mcp", Arg2pLanguageServer.STACK_SIZE).apply {
                isDaemon =
                    true
            }
        }

    /** Answers the messages of the client until it closes the input */
    fun run() {
        while (true) {
            val line = reader.readLine() ?: break
            if (line.isBlank()) continue
            val message =
                try {
                    JsonParser.parseString(line).asJsonObject
                } catch (_: Exception) {
                    respond(JsonNull.INSTANCE, error = error(-32700, "Parse error"))
                    continue
                }
            handle(message)
        }
        solving.shutdownNow()
    }

    private fun handle(message: JsonObject) {
        val id = message.get("id") ?: return // a notification (notifications/initialized, ...): nothing to answer
        val params = message.getAsJsonObject("params") ?: JsonObject()
        when (val method = message.get("method")?.asString) {
            "initialize" -> respond(id, initialize(params))
            "ping" -> respond(id, JsonObject())
            "tools/list" -> respond(id, JsonObject().apply { add("tools", gson.toJsonTree(TOOLS)) })
            "tools/call" -> respond(id, call(params))
            "resources/list" -> respond(id, JsonObject().apply { add("resources", gson.toJsonTree(listOf(RESOURCE))) })
            "resources/templates/list" -> respond(id, JsonObject().apply { add("resourceTemplates", JsonArray()) })
            "resources/read" -> {
                val uri = params.get("uri")?.asString
                if (uri == McpApp.URI) {
                    respond(id, JsonObject().apply { add("contents", gson.toJsonTree(listOf(RESOURCE + ("text" to McpApp.page)))) })
                } else {
                    respond(id, error = error(-32002, "Resource not found: $uri"))
                }
            }
            else -> respond(id, error = error(-32601, "Method not found: $method"))
        }
    }

    private fun initialize(params: JsonObject): JsonObject {
        val asked = params.get("protocolVersion")?.asString
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

    // The result of a tool, as MCP gives it: its JSON as text and as structured content, or its error
    private fun call(params: JsonObject): JsonObject {
        val name = params.get("name")?.asString
        val arguments = params.getAsJsonObject("arguments") ?: JsonObject()
        val timeout = arguments.get("timeoutSeconds")?.asLong ?: DEFAULT_TIMEOUT
        val future =
            solving.submit<Any?> {
                when (name) {
                    "solve" -> solve(arguments)
                    "answer" -> answer(arguments)
                    "check" -> check(arguments)
                    "settings" -> settings(arguments)
                    "explore" -> explore(arguments)
                    "engine" ->
                        mapOf(
                            "answers" to app.handle(arguments.text("session") ?: "", arguments.getAsJsonObject("request") ?: JsonObject()),
                        )
                    else -> throw IllegalArgumentException("Unknown tool: $name")
                }
            }
        return try {
            val result = gson.toJsonTree(future.get(timeout, TimeUnit.SECONDS))
            JsonObject().apply {
                add("content", content(if (name == "explore") explored(result.asJsonObject) else gson.toJson(result)))
                if (result.isJsonObject) add("structuredContent", result)
                addProperty("isError", false)
            }
        } catch (_: TimeoutException) {
            future.cancel(true)
            failure("No answer within $timeout seconds: the theory may not terminate, or need a larger timeoutSeconds.")
        } catch (failed: ExecutionException) {
            val cause = failed.cause ?: failed
            val syntax = (cause as? Arg2pException)?.syntax?.let { " (line ${it.line}, column ${it.column})" } ?: ""
            failure((cause.message ?: cause.toString()) + syntax)
        }
    }

    private fun solve(arguments: JsonObject): Map<String, Any?> {
        val solver = solver(arguments)
        val query = arguments.text("query") ?: DEFAULT_QUERY
        val most = arguments.get("maxSolutions")?.asInt ?: DEFAULT_SOLUTIONS
        val solutions = solver.solve(query)
        val found = mutableListOf<Map<String, Any?>>()
        var graph = GraphData.EMPTY
        val profile = solver.setting("graphBuildMode").firstOrNull() ?: ""
        while (found.size < most && solutions.hasNext()) {
            val solution = solutions.next()
            found +=
                mapOf(
                    "result" to
                        if (solution.isYes) {
                            "yes"
                        } else if (solution.isNo) {
                            "no"
                        } else {
                            "halt"
                        },
                    "query" to solution.query,
                    "bindings" to solution.variables.associateWith { solution.text(it) },
                    "error" to solution.error,
                )
            if (solution.isYes) graph = Engine.graphData(solver.graph(), profile)
        }
        return mapOf(
            "profile" to profile,
            "solutions" to found,
            "more" to solutions.hasNext(),
            "graph" to graph,
        )
    }

    private fun answer(arguments: JsonObject): Map<String, Any?> {
        val answer = solver(arguments).answer(arguments.text("query") ?: throw IllegalArgumentException("A query is needed"))
        return mapOf(
            "in" to answer.inStatements.map { it.toString() },
            "out" to answer.outStatements.map { it.toString() },
            "und" to answer.undStatements.map { it.toString() },
        )
    }

    private fun check(arguments: JsonObject): Map<String, Any?> {
        val problem = Engine.check(arguments.text("text") ?: "", arguments.text("kind") ?: "theory")
        return mapOf("parses" to (problem == null), "problem" to problem)
    }

    private fun settings(arguments: JsonObject): Map<String, Any?> = Engine.settings(arguments.text("theory") ?: "", flagsText(arguments))

    // What the solver is shown with: the theory, if it parses, its query, its flags, and the link to the playground
    private fun explore(arguments: JsonObject): Map<String, Any?> {
        val theory = arguments.text("theory") ?: ""
        Engine.check(theory, "theory")?.let {
            throw IllegalArgumentException("The theory does not parse at line ${it.line}, column ${it.column}: ${it.message}")
        }
        val query = arguments.text("query") ?: DEFAULT_QUERY
        val flags = flagsText(arguments)
        return mapOf(
            "theory" to theory,
            "query" to query,
            "flags" to flags,
            "profile" to (Engine.settings(theory, flags)["graphBuildMode"]?.firstOrNull() ?: ""),
            "playground" to Playground.link(theory, query, flags),
        )
    }

    private fun explored(result: JsonObject): String =
        "The solver of the theory is shown to the user, where this client shows MCP Apps: they can run the query " +
            "(${result.text("query")}), browse its solutions, their argument graphs and stages, and change the flags. " +
            "The same theory opens in the web playground: ${result.text("playground")}"

    private fun solver(arguments: JsonObject): Arg2p =
        Arg2p
            .builder()
            .theory(arguments.text("theory") ?: "")
            .flags(Flags.parse(flagsText(arguments)))
            .output { }
            .build()

    private fun flagsText(arguments: JsonObject): String = arguments.text("flags") ?: DEFAULT_FLAGS

    private fun JsonObject.text(name: String): String? = get(name)?.takeUnless { it.isJsonNull }?.asString

    private fun content(text: String): JsonArray =
        JsonArray().apply {
            add(
                JsonObject().apply {
                    addProperty("type", "text")
                    addProperty("text", text)
                },
            )
        }

    private fun failure(message: String): JsonObject =
        JsonObject().apply {
            add("content", content(message))
            addProperty("isError", true)
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
        writer.println(gson.toJson(response))
    }

    companion object {
        /** The versions of the protocol the server speaks, the latest first */
        val PROTOCOL_VERSIONS = listOf("2025-06-18", "2025-03-26", "2024-11-05")

        const val DEFAULT_QUERY = "buildLabelSets(In, Out, Und)"
        const val DEFAULT_FLAGS = "argumentLabellingMode(grounded).\nstatementLabellingMode(statement).\nqueryMode.\n"
        const val DEFAULT_SOLUTIONS = 5
        const val DEFAULT_TIMEOUT = 60L

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

        private val TOOLS =
            listOf(
                mapOf(
                    "name" to "solve",
                    "title" to "Solve a query",
                    "description" to
                        "Solves a query on a theory of arg2p: its first solutions (yes, no, or halt with an error), with " +
                        "their bindings, and the argument graph of the last yes (every argument with its conclusion, top " +
                        "rule, sub-arguments and label in, out or und; every attack with its type). The default query, " +
                        "buildLabelSets(In, Out, Und), gives the statements labelled in, out and und.",
                    "inputSchema" to
                        schema(
                            "theory" to text(THEORY),
                            "query" to text("A Prolog goal on the theory (default: $DEFAULT_QUERY)"),
                            "flags" to text(FLAGS),
                            "maxSolutions" to integer("How many solutions at most (default $DEFAULT_SOLUTIONS)"),
                            "timeoutSeconds" to integer("How long to wait (default $DEFAULT_TIMEOUT)"),
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
                            "timeoutSeconds" to integer("How long to wait (default $DEFAULT_TIMEOUT)"),
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
                                    "enum" to listOf("theory", "query"),
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
                        "graph extensions, ... as the theory, the flags and the profile set them.",
                    "inputSchema" to schema("theory" to text(THEORY), "flags" to text(FLAGS), required = listOf("theory")),
                ),
                mapOf(
                    "name" to "explore",
                    "title" to "Show the solver of a theory",
                    "description" to
                        "Shows the user the solver of a theory, to explore its results themselves: the query run, its " +
                        "solutions, the argument graph of each and the stages of its computation, the drawing of an " +
                        "abstract framework, the flags. Hosts of MCP Apps show it in the conversation; the result links the " +
                        "same theory in the web playground too. Use it when the user wants to see or explore the results.",
                    "inputSchema" to
                        schema(
                            "theory" to text(THEORY),
                            "query" to text("The query run first (default: $DEFAULT_QUERY)"),
                            "flags" to text(FLAGS),
                            required = listOf("theory"),
                        ),
                    "_meta" to
                        mapOf(
                            "ui" to mapOf("resourceUri" to McpApp.URI, "visibility" to listOf("model", "app")),
                            // the key of the first hosts of MCP Apps
                            "ui/resourceUri" to McpApp.URI,
                        ),
                ),
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
                ),
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
