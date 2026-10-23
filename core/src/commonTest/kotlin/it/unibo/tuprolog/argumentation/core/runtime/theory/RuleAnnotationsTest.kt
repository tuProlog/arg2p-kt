package it.unibo.tuprolog.argumentation.core.runtime.theory

import it.unibo.tuprolog.argumentation.core.Arg2pSolver
import kotlin.test.Test
import kotlin.test.assertEquals

class RuleAnnotationsTest {
    private fun identifiers(text: String) =
        RuleAnnotations
            .parse(text, Arg2pSolver.default().operators())
            .clauses
            .map { RuleAnnotations.of(it) }
            .toList()

    @Test
    fun theCommentAfterTheFullStop() {
        assertEquals(listOf("r1", null), identifiers("a :- b. % r1\nb."))
    }

    @Test
    fun aClauseOnManyLines() {
        assertEquals(listOf("r2", "f1"), identifiers("a :-\n    b,\n    c. % r2\nb. %f1"))
    }

    @Test
    fun otherCommentsAreNotIdentifiers() {
        // a comment of many words, one on the line after, one before the full stop, a % in a quoted atom
        val text = "a :- b. % the rule of a\nb.\n% r3\nc :- d % r4\n    .\nd('%r5')."
        assertEquals(listOf(null, null, null, null), identifiers(text))
    }
}
