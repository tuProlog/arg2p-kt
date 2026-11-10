package it.unibo.tuprolog.argumentation.core.libraries.framework.store

import it.unibo.tuprolog.argumentation.core.LibraryTesting
import it.unibo.tuprolog.argumentation.core.libraries.framework.store.helpers.ArgumentTerm
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.parsing.parse
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertNull
import kotlin.test.assertTrue

class ArgumentTermTest {
    private val module = LibraryTesting()

    // r2 on top of r1, defeasible, concluding b from a
    private val argument = "[[r2, r1], r2, [b], [a], [[r2], [r2, r1], [f1]]]"
    private val premise = "[[f1], none, [a], [], [[], [], [f1]]]"
    private val strict = "[[s1], s1, [c], [], [[], [], []]]"

    @Test
    fun accessorsReadTheParts() {
        assertEquals("[r2, r1]", module.value("store::argumentRules($argument, X)", "X"))
        assertEquals("r2", module.value("store::topRule($argument, X)", "X"))
        assertEquals("[b]", module.value("store::conclusion($argument, X)", "X"))
        assertEquals("b", module.value("store::claim($argument, X)", "X"))
        assertEquals("[a]", module.value("store::body($argument, X)", "X"))
        assertEquals("[[r2], [r2, r1], [f1]]", module.value("store::argumentInfo($argument, X)", "X"))
        assertEquals("[r2]", module.value("store::lastDefeasibleRules($argument, X)", "X"))
    }

    @Test
    fun newBuildsWhatTheAccessorsRead() {
        assertEquals(
            Term.parse(argument),
            Term.parse(module.value("store::infoParts(I, [r2], [r2, r1], [f1]), store::newArgument([r2, r1], r2, [b], [a], I, A)", "A")!!),
        )
        assertTrue(module.holds("store::newArgument(R, T, C, B, I, $argument), R == [r2, r1], T == r2, C == [b], B == [a]"))
    }

    @Test
    fun accessorsBuildTemplates() {
        // the template of the arguments concluding b, to look them up
        assertTrue(module.holds("store::claim(A, b), A = $argument"))
        assertFalse(module.holds("store::claim(A, c), A = $argument"))
    }

    @Test
    fun withRulesChangesTheRulesOnly() {
        assertEquals(
            Term.parse("[[x|[r2, r1]], r2, [b], [a], [[r2], [r2, r1], [f1]]]"),
            Term.parse(module.value("store::argumentRules($argument, R), store::withArgumentRules($argument, [x|R], A)", "A")!!),
        )
    }

    @Test
    fun premisesAndDefeasibility() {
        assertTrue(module.holds("store::isPremise($premise)"))
        assertFalse(module.holds("store::isPremise($argument)"))
        // a variable top rule is not taken for none
        assertFalse(module.holds("store::isPremise([[x], _, [a], [], [[], [], []]])"))
        assertTrue(module.holds("store::isDefeasible($argument)"))
        assertTrue(module.holds("store::isDefeasible($premise)"))
        assertFalse(module.holds("store::isDefeasible($strict)"))
    }

    @Test
    fun kotlinReadsTheSameParts() {
        val parts = ArgumentTerm.parts(Term.parse(argument))!!
        assertEquals("[r2, r1]", parts.rules.toString())
        assertEquals("r2", parts.topRule.toString())
        assertEquals("b", parts.claim.toString())
        assertEquals("[a]", parts.body.toString())
        assertEquals("[r2]", parts.lastDefeasibleRules.toString())
        assertEquals("[r2, r1]", parts.defeasibleRules.toString())
        assertEquals("[f1]", parts.defeasiblePremises.toString())
        assertEquals(
            Term.parse("[[r2], [r2, r1], [f1]]"),
            ArgumentTerm.info(parts.lastDefeasibleRules, parts.defeasibleRules, parts.defeasiblePremises),
        )
        assertNull(ArgumentTerm.parts(Term.parse("[a, b]")))
        assertNull(ArgumentTerm.parts(Term.parse("[r, t, [c], [], [x]]")))
    }
}
