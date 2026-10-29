package it.unibo.tuprolog.argumentation.lsp

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

    /** The page of the solver */
    val page: String by lazy {
        Pages.render(
            "solver.html",
            mapOf(
                "ui" to Pages.UI_VERSION,
                "cdn" to CDN,
                "query" to Pages.script(Solvers.DEFAULT_QUERY),
                "emptyGraph" to Pages.script(GraphData.EMPTY),
            ),
        )
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
