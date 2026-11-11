package it.unibo.tuprolog.argumentation.core.libraries.framework.parser.helpers

import it.unibo.tuprolog.argumentation.engine.InternalArg2pApi
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.operators.OperatorSet
import it.unibo.tuprolog.core.parsing.parse

/**
 * A theory as text, before it is read: its clauses as written, and the facts among them stating flags, read with the
 * notation of Prolog alone (see [TheoryNotation], which picks the notation the whole theory is read with from them).
 */
@InternalArg2pApi
object TheoryText {
    /**
     * The clauses of [text], each as written without its final dot: a clause ends at a dot followed by a layout character,
     * a comment or the end, that is no part of a longer symbol (=..), of a quoted atom, a string, a character code (0'.), a
     * number (1.5) or a comment
     */
    fun clauses(text: String): List<String> {
        val clauses = ArrayList<String>()
        var start = 0
        var i = 0
        val n = text.length
        while (i < n) {
            val c = text[i]
            when {
                c == '%' -> i = text.indexOf('\n', i).let { if (it < 0) n else it }
                c == '/' && i + 1 < n && text[i + 1] == '*' -> i = text.indexOf("*/", i + 2).let { if (it < 0) n else it + 2 }
                c == '\'' || c == '"' || c == '`' -> i = quotedEnd(text, i)
                c == '0' && i + 1 < n && text[i + 1] == '\'' -> i = minOf(n, i + (if (i + 2 < n && text[i + 2] == '\\') 4 else 3))
                c.isDigit() -> {
                    i++
                    while (i < n &&
                        (text[i].isLetterOrDigit() || text[i] == '_' || (text[i] == '.' && i + 1 < n && text[i + 1].isDigit()))
                    ) {
                        i++
                    }
                }
                c in SYMBOLS -> {
                    val from = i
                    while (i < n && text[i] in SYMBOLS) i++
                    if (i - from == 1 && c == '.' && (i == n || text[i].isWhitespace() || text[i] == '%')) {
                        text
                            .substring(start, from)
                            .trim()
                            .takeIf { it.isNotEmpty() }
                            ?.let { clauses += it }
                        start = i
                    }
                }
                else -> i++
            }
        }
        text
            .substring(start)
            .trim()
            .takeIf { it.isNotEmpty() && !onlyComments(it) }
            ?.let { clauses += it }
        return clauses
    }

    /** The facts of [text] whose predicate is among [names], each read with the notation of Prolog alone: a clause that does not read so is none of them */
    fun facts(
        text: String,
        names: Set<String>,
    ): List<Struct> =
        clauses(text)
            .mapNotNull { clause ->
                val name = clause.trimStart().takeWhile { it.isLetterOrDigit() || it == '_' }
                if (name !in names) return@mapNotNull null
                runCatching { Term.parse(clause, OperatorSet.DEFAULT) }.getOrNull() as? Struct
            }.filter { it.functor in names }

    // The end of the quoted text starting at [from]: its closing quote (doubled quotes and escapes inside), or the end
    private fun quotedEnd(
        text: String,
        from: Int,
    ): Int {
        val quote = text[from]
        var i = from + 1
        while (i < text.length) {
            when {
                text[i] == '\\' -> i += 2
                text[i] == quote && i + 1 < text.length && text[i + 1] == quote -> i += 2
                text[i] == quote -> return i + 1
                else -> i++
            }
        }
        return text.length
    }

    private fun onlyComments(text: String) = text.lines().all { it.isBlank() || it.trimStart().startsWith("%") }

    private const val SYMBOLS = "+-*/\\^<>=~:.?@#&$"
}
