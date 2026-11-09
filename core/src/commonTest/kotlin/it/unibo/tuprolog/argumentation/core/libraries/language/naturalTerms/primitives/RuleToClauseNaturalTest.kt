package it.unibo.tuprolog.argumentation.core.libraries.language.naturalTerms.primitives

import kotlin.test.Test
import kotlin.test.assertEquals

// The statements written as text (naturalTerms) matched against the templates of those before them: the text around
// their words in upper case is matched as it is, whatever the characters it has
class RuleToClauseNaturalTest {
    @Test
    fun theTextOfATemplateIsMatchedAsItIs() {
        val extractor = RuleToClauseNatural.TemplateExtractor()
        extractor.processInternal("X ** Y")
        assertEquals(listOf("a", "b"), extractor.processInternal("a ** b").extracted)
        extractor.processInternal("X is happy.")
        assertEquals(emptyList(), extractor.processInternal("bob is happy!").extracted)
        assertEquals(listOf("bob"), extractor.processInternal("bob is happy.").extracted)
    }
}
