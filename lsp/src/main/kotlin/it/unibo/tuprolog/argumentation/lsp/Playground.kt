package it.unibo.tuprolog.argumentation.lsp

import com.google.gson.GsonBuilder
import java.io.ByteArrayOutputStream
import java.util.Base64
import java.util.zip.Deflater

/**
 * Links to the web playground opening a theory, as the playground shares them: the JSON array `[2, flags, theory,
 * query]`, deflated with the dictionary of the version 2 of its links, base64url encoded, in the parameter `s` (see
 * links.ts in arg2p-kt-web).
 */
object Playground {
    /** The web playground */
    const val URL = "https://tuprolog.github.io/arg2p-kt-web/"

    private const val VERSION = 2

    // The dictionary of the version 2 of the links of the playground, byte for byte: never to be edited
    private val DICTIONARY =
        listOf(
            "[2,\"",
            "buildLabelSets(In, Out, Und)",
            "graphBuildMode(abstract).\\n",
            "graphBuildMode(aba).\\n",
            "graphBuildMode(aspic).\\n",
            "argumentLabellingMode(grounded).\\n",
            "argumentLabellingMode(preferred).\\n",
            "statementLabellingMode(statement).\\n",
            "queryMode.\\n",
            "assumption(",
            "contrary(",
            " :=> ",
            " => ",
            " :- ",
            " -> ",
            " <-> ",
            "(X)",
            "(X, Y)",
            "\\n\\n",
        ).joinToString("").toByteArray(Charsets.UTF_8)

    private val gson = GsonBuilder().disableHtmlEscaping().create()

    /** The link opening [theory], [query] and [flags] in the playground */
    fun link(
        theory: String,
        query: String,
        flags: String,
    ): String {
        val state = gson.toJson(listOf(VERSION, flags, theory, query)).toByteArray(Charsets.UTF_8)
        val deflater = Deflater(Deflater.BEST_COMPRESSION, true)
        val packed =
            try {
                deflater.setDictionary(DICTIONARY)
                deflater.setInput(state)
                deflater.finish()
                val bytes = ByteArrayOutputStream()
                val buffer = ByteArray(4096)
                while (!deflater.finished()) bytes.write(buffer, 0, deflater.deflate(buffer))
                bytes.toByteArray()
            } finally {
                deflater.end()
            }
        return URL + "?s=" + Base64.getUrlEncoder().withoutPadding().encodeToString(packed)
    }
}
