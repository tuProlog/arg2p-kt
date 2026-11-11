package it.unibo.tuprolog.argumentation.lsp

import org.eclipse.lsp4j.DiagnosticSeverity
import kotlin.test.Test
import kotlin.test.assertEquals

/** The words of a theory spelled as before 0.26.0 are hints, each with its quick fix; not those in comments or quotes */
class EarlierSpellingHintsTest {
    private val theory = "graphBuildMode(aspic_plus). % semistable\nr1 :=> 'bp_grounded'.\nq :- causality::but_for(a, b)."

    @Test
    fun theWordsSpelledAsBeforeAreHints() {
        val hints = EarlierSpellingHints.of(theory)
        val words = hints.map { theory.lines()[it.range.start.line].substring(it.range.start.character, it.range.end.character) }
        assertEquals(listOf("aspic_plus", "but_for"), words)
        assertEquals(setOf(DiagnosticSeverity.Hint), hints.map { it.severity }.toSet())
        assertEquals(hints, Arg2pLanguageServer.diagnostics(theory))
    }

    @Test
    fun theirQuickFixesWriteTheCurrentSpelling() {
        val fixes = EarlierSpellingHints.fixes("file:///t.arg2p", theory, EarlierSpellingHints.of(theory))
        assertEquals(
            listOf("aspicPlus", "butFor"),
            fixes.map {
                it.edit.changes
                    .getValue("file:///t.arg2p")
                    .single()
                    .newText
            },
        )
    }
}
