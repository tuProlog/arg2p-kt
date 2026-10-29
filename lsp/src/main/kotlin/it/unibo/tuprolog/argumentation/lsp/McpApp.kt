package it.unibo.tuprolog.argumentation.lsp

import java.util.Properties

/**
 * The solver pane of the MCP server, an MCP App (the extension io.modelcontextprotocol/ui of MCP): the page of the
 * solver, served as the resource [URI], which the hosts of MCP Apps show when the tool explore is called. The page
 * solves on the engine of the pages (see [PageEngine]), through the tool engine, which only the page calls; each page
 * is a session of its own.
 */
internal object McpApp {
    /** The resource of the page of the solver */
    const val URI = "ui://arg2p/solver"

    /** The type of the resources of MCP Apps */
    const val MIME_TYPE = "text/html;profile=mcp-app"

    /** The extension of MCP the apps are */
    const val EXTENSION = "io.modelcontextprotocol/ui"

    /** Where the page loads its components and their dependencies from */
    const val CDN = "https://esm.sh"

    // The version of what the page loads, the one the extension of VS Code is built with (see the build of the server)
    private val versions =
        Properties().apply {
            McpApp::class.java.getResourceAsStream("page.properties")?.use(::load)
                ?: error("The version of the components of the page is missing from the jar")
        }

    /** The version of @tuprolog/arg2p-ui the page loads (ARG2P_UI_VERSION another one, to try it) */
    val UI_VERSION: String = System.getenv("ARG2P_UI_VERSION")?.takeIf { it.isNotBlank() } ?: versions.getProperty("ui")

    /** The page of the solver */
    val page: String by lazy {
        val text =
            McpApp::class.java
                .getResourceAsStream("solver.html")
                ?.use { it.readBytes().toString(Charsets.UTF_8) }
                ?: error("The page of the solver is missing from the jar")
        text
            .replace("{{ui}}", UI_VERSION)
            .replace("{{cdn}}", CDN)
            .replace("{{query}}", json.toJson(Solvers.DEFAULT_QUERY))
            .replace("{{emptyGraph}}", json.toJson(GraphData.EMPTY))
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
