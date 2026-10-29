package it.unibo.tuprolog.argumentation.lsp

import java.io.ByteArrayOutputStream
import java.util.Base64
import java.util.zip.Inflater

/** The state [packed] in the parameter `s` of a link of the playground, as its JSON (`[2, flags, theory, query]`) */
internal fun Playground.state(packed: String): String {
    val inflater = Inflater(true)
    return try {
        inflater.setInput(Base64.getUrlDecoder().decode(packed))
        // a raw stream, with no header asking for its dictionary
        inflater.setDictionary(DICTIONARY)
        val bytes = ByteArrayOutputStream()
        val buffer = ByteArray(4096)
        while (!inflater.finished()) {
            val read = inflater.inflate(buffer)
            if (read == 0 && inflater.needsInput()) error("A link of the playground cut short")
            bytes.write(buffer, 0, read)
        }
        bytes.toString(Charsets.UTF_8)
    } finally {
        inflater.end()
    }
}
