package it.unibo.tuprolog.argumentation.lsp

import java.util.Properties

/**
 * The pages of the servers, as the jar has them: the templates of the solver (solver.html, the MCP App, see [McpApp];
 * recorded.html, a run already solved, see [RecordedPage]), the style they share (solver.css), and the version of the
 * components of @tuprolog/arg2p-ui they load; and the page joining a federation (join.html, see [Federation]).
 */
internal object Pages {
    // The version of what the pages load, the one the extension of VS Code is built with (see the build of the server)
    private val versions =
        Properties().apply {
            Pages::class.java.getResourceAsStream("page.properties")?.use(::load)
                ?: error("The version of the components of the page is missing from the jar")
        }

    /** The version of @tuprolog/arg2p-ui the pages load (ARG2P_UI_VERSION another one, to try it) */
    val UI_VERSION: String = System.getenv("ARG2P_UI_VERSION")?.takeIf { it.isNotBlank() } ?: versions.getProperty("ui")

    // A placeholder of a template, {{name}}
    private val PLACEHOLDER = Regex("""\{\{(\w+)}}""")

    /** The style of the pages, which each inlines as {{style}} */
    val style: String by lazy { resource("solver.css") }

    /**
     * The page of the template [name], each of its placeholders ({{name}}) replaced by its text in [values], the style
     * of the pages as {{style}}: in one pass, the placeholders a text may hold are left as they are
     */
    fun render(
        name: String,
        values: Map<String, String>,
    ): String {
        val all = values + ("style" to style)
        return PLACEHOLDER.replace(resource(name)) { all[it.groupValues[1]] ?: it.value }
    }

    /** The resource [name] of the pages, as text */
    fun resource(name: String): String =
        Pages::class.java
            .getResourceAsStream(name)
            ?.use { it.readBytes().toString(Charsets.UTF_8) }
            ?: error("The resource $name of the pages is missing from the jar")

    /**
     * [value] as JSON within a script of a page: no <, > or & in it, which could close the script (</script>) or open a
     * comment (<!--); escaped in the texts of the JSON, the only place they can be, they read the same
     */
    fun script(value: Any?): String =
        json
            .toJson(value)
            .replace("<", "\\u003c")
            .replace(">", "\\u003e")
            .replace("&", "\\u0026")
}
