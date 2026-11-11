package it.unibo.tuprolog.argumentation.lsp

import it.unibo.tuprolog.argumentation.EarlierSpellings
import org.eclipse.lsp4j.CodeAction
import org.eclipse.lsp4j.CodeActionKind
import org.eclipse.lsp4j.Diagnostic
import org.eclipse.lsp4j.DiagnosticSeverity
import org.eclipse.lsp4j.Position
import org.eclipse.lsp4j.Range
import org.eclipse.lsp4j.TextEdit
import org.eclipse.lsp4j.WorkspaceEdit

/**
 * The words of a theory spelled as arg2p did before 1.0.0 (see [EarlierSpellings]): read as before, each is a hint with the
 * spelling of 1.0.0, and its quick fix writes it. The words in comments and quoted text are left alone.
 */
internal object EarlierSpellingHints {
    /** The code of the hints */
    const val CODE = "earlierSpelling"

    /** The hints of [text] */
    fun of(text: String): List<Diagnostic> =
        words(text).mapNotNull { (range, word) ->
            EarlierSpellings.current(word)?.let { current ->
                Diagnostic(
                    range,
                    "Written `$current` since arg2p 1.0.0 (`$word` is the spelling of earlier versions, still read)",
                    DiagnosticSeverity.Hint,
                    "arg2p",
                    CODE,
                )
            }
        }

    /** The quick fixes of the hints of [diagnostics] in [text], the document [uri]: the spelling of 1.0.0 written */
    fun fixes(
        uri: String,
        text: String,
        diagnostics: List<Diagnostic>,
    ): List<CodeAction> {
        val lines = text.split('\n')
        return diagnostics.filter { it.code?.left == CODE }.mapNotNull { hint ->
            val (start, end) = hint.range.start to hint.range.end
            val word = lines.getOrNull(start.line)?.takeIf { end.character <= it.length }?.substring(start.character, end.character)
            val current = word?.let(EarlierSpellings::current) ?: return@mapNotNull null
            CodeAction("Write `$current`").apply {
                kind = CodeActionKind.QuickFix
                this.diagnostics = listOf(hint)
                isPreferred = true
                edit = WorkspaceEdit(mapOf(uri to listOf(TextEdit(hint.range, current))))
            }
        }
    }

    // The words of [text] starting with a lowercase letter (atoms, the names of predicates), with where they are, but
    // those in comments (% and /* */) and in quotes
    private fun words(text: String): List<Pair<Range, String>> {
        val found = ArrayList<Pair<Range, String>>()
        var block = false
        text.split('\n').forEachIndexed { line, content ->
            var at = 0
            var quote: Char? = null
            while (at < content.length) {
                val char = content[at]
                when {
                    block -> if (content.startsWith("*/", at)) block = false.also { at++ }
                    quote != null ->
                        if (char == '\\') {
                            at++
                        } else if (char == quote) {
                            quote = null
                        }
                    char == '%' -> break
                    content.startsWith("/*", at) -> block = true.also { at++ }
                    char == '\'' || char == '"' || char == '`' -> quote = char
                    char in 'a'..'z' && (at == 0 || !content[at - 1].isWordPart()) -> {
                        val end = (at until content.length).firstOrNull { !content[it].isWordPart() } ?: content.length
                        found += Range(Position(line, at), Position(line, end)) to content.substring(at, end)
                        at = end - 1
                    }
                }
                at++
            }
        }
        return found
    }

    private fun Char.isWordPart() = isLetterOrDigit() || this == '_'
}
