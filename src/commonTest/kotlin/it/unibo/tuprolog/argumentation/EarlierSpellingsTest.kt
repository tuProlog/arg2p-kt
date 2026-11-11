package it.unibo.tuprolog.argumentation

import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertTrue

/** A theory written as the papers on arg2p wrote it, before 0.26.0, runs as it did: its spellings read as the current ones */
class EarlierSpellingsTest {
    private val burden = "r1 :=> a.\nr2 :=> -a.\nbp(a).\n"

    @Test
    fun theValuesOfTheFlags() {
        val earlier = Arg2p.of("graphBuildMode(aspic_plus).\nargumentLabellingMode(semistable).\nstatementLabellingMode(statement_binary).")
        assertEquals(listOf("aspicPlus"), earlier.setting("graphBuildMode"))
        assertEquals(listOf("semiStable"), earlier.setting("argumentLabellingMode"))
        assertEquals(listOf("statementBinary"), earlier.setting("statementLabellingMode"))
        assertEquals(listOf("grounded"), Arg2p.of("argumentLabellingMode(grounded_hash).").setting("argumentLabellingMode"))
        assertEquals(listOf("aspicPlus"), Arg2p.of("graphBuildMode(standard_af).").setting("graphBuildMode"))
    }

    @Test
    fun aSemanticsNamedAsBefore() {
        val current = Arg2p.of("${burden}argumentLabellingMode(bpGrounded).").answer("a").single()
        assertEquals(current, Arg2p.of("${burden}argumentLabellingMode(bp_grounded).").answer("a").single())
        assertEquals(current, Arg2p.of(burden, Flags.parse("argumentLabellingMode(bp_grounded).")).answer("a").single())
    }

    @Test
    fun thePredicatesNamedAsBefore() {
        val arg2p = Arg2p.of("r1 :=> a.\nr2 : a => b.")
        assertTrue(arg2p.solve("causality::butFor(a, b)").next().isYes)
        assertTrue(arg2p.solve("causality::but_for(a, b)").next().isYes)
        assertTrue(arg2p.solve("causality::ness_intervention([-a], b)").next().isYes)
        assertTrue(arg2p.solve("solve, context_check(statIn(_)), contextCheck(statIn(_))").next().isYes)
    }

    @Test
    fun theTableMapsEachToItsCurrentName() {
        EarlierSpellings.predicates.forEach { (earlier, current) -> assertTrue('_' in earlier && '_' !in current, earlier) }
        assertEquals("butFor", EarlierSpellings.current("but_for"))
        assertEquals("contextAssertId", EarlierSpellings.current("context_assert_id"))
    }
}
