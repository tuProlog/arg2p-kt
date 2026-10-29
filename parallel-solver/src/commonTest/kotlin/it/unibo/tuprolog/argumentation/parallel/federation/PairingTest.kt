package it.unibo.tuprolog.argumentation.parallel.federation

import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertNotEquals
import kotlin.test.assertNotNull
import kotlin.test.assertNull
import kotlin.test.assertTrue

// The code of a site, the pairings made with it, and the wrong codes of an address
class PairingTest {
    private val digits = Regex("""\d{4}""")

    @Test
    fun aCodeIsFourDigits() {
        val pairing = Pairing()
        assertTrue(digits.matches(pairing.code))
        repeat(5) { assertTrue(digits.matches(pairing.renew())) }
    }

    @Test
    fun theCodePairsASiteWhichPresentsItsTokenFromThen() {
        val pairing = Pairing()
        assertNull(pairing.refusal("site-1", "10.0.0.2", pairing.code))
        val token = assertNotNull(pairing.tokenOf("site-1"))
        assertEquals(setOf("site-1"), pairing.paired)
        pairing.renew()
        assertNull(pairing.refusal("site-1", "10.0.0.2", token))
        // the token of a site is its own
        assertNotNull(pairing.refusal("site-2", "10.0.0.3", token))
    }

    @Test
    fun wrongCodesAreRefusedForAWhileAfterAFew() {
        val pairing = Pairing()
        val wrong = if (pairing.code == "0000") "0001" else "0000"
        repeat(3) { assertEquals("Wrong code", pairing.refusal("site-1", "10.0.0.2", wrong)) }
        // the right code as well, from that address, for a while; not from another address
        assertNotEquals(null, pairing.refusal("site-1", "10.0.0.2", pairing.code))
        assertNull(pairing.refusal("site-2", "10.0.0.3", pairing.code))
        // a site paired is admitted by its token all the same
        val token = assertNotNull(pairing.tokenOf("site-2"))
        assertNull(pairing.refusal("site-2", "10.0.0.2", token))
        assertNotNull(pairing.refusal("site-1", "10.0.0.2", null))
    }
}
