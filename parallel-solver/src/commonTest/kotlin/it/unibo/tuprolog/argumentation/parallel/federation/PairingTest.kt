package it.unibo.tuprolog.argumentation.parallel.federation

import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertIs
import kotlin.test.assertNotEquals
import kotlin.test.assertNotNull
import kotlin.test.assertNull
import kotlin.test.assertTrue

// The code of a site, the pairings made with it, and the wrong codes of an address
class PairingTest {
    private val digits = Regex("""\d{4}""")

    // What [site] is told presenting the proof of [secret] on a nonce of [pairing]
    private fun admission(
        pairing: Pairing,
        site: String,
        address: String?,
        secret: String?,
    ): Pairing.Admission {
        val nonce = Pairing.newNonce()
        return pairing.admission(site, address, nonce, secret?.let { Pairing.proof(it, site, nonce) })
    }

    @Test
    fun aCodeIsFourDigits() {
        val pairing = Pairing()
        assertTrue(digits.matches(pairing.code))
        repeat(5) { assertTrue(digits.matches(pairing.renew())) }
    }

    @Test
    fun theCodePairsASiteWhichProvesItsTokenFromThen() {
        val pairing = Pairing()
        val code = pairing.code
        val paired = assertIs<Pairing.Proven>(admission(pairing, "site-1", "10.0.0.2", code))
        val token = assertNotNull(pairing.tokenOf("site-1"))
        assertEquals(token, paired.token)
        assertEquals(code, paired.secret)
        assertEquals(setOf("site-1"), pairing.paired)
        // the code renewed once a site paired: the one before refused
        assertNotEquals(code, pairing.code)
        assertIs<Pairing.Refusal>(admission(pairing, "site-3", "10.0.0.4", code))
        // the token, with no new one
        val again = assertIs<Pairing.Proven>(admission(pairing, "site-1", "10.0.0.2", token))
        assertNull(again.token)
        assertEquals(token, again.secret)
        // the token of a site is its own
        assertIs<Pairing.Refusal>(admission(pairing, "site-2", "10.0.0.3", token))
    }

    @Test
    fun aProofIsOfItsNonceAndItsSiteAlone() {
        val pairing = Pairing()
        val nonce = Pairing.newNonce()
        val proof = Pairing.proof(pairing.code, "site-1", nonce)
        // the proof tells nothing of the code, and does not serve on another nonce or for another site
        assertTrue(pairing.code !in proof)
        assertIs<Pairing.Refusal>(pairing.admission("site-1", "10.0.0.2", Pairing.newNonce(), proof))
        assertIs<Pairing.Refusal>(pairing.admission("site-2", "10.0.0.3", nonce, proof))
        assertIs<Pairing.Proven>(pairing.admission("site-1", "10.0.0.4", nonce, proof))
    }

    @Test
    fun wrongCodesAreRefusedForAWhileAfterAFew() {
        val pairing = Pairing()
        val wrong = if (pairing.code == "0000") "0001" else "0000"
        repeat(3) { assertEquals("Wrong code", assertIs<Pairing.Refusal>(admission(pairing, "site-1", "10.0.0.2", wrong)).reason) }
        // the right code as well, from that address, for a while; not from another address
        assertIs<Pairing.Refusal>(admission(pairing, "site-1", "10.0.0.2", pairing.code))
        assertIs<Pairing.Proven>(admission(pairing, "site-2", "10.0.0.3", pairing.code))
        // a site paired is admitted by its token all the same
        val token = assertNotNull(pairing.tokenOf("site-2"))
        assertIs<Pairing.Proven>(admission(pairing, "site-2", "10.0.0.2", token))
        assertIs<Pairing.Refusal>(admission(pairing, "site-1", "10.0.0.2", null))
    }

    // the links with no address share no count of wrong codes: the code is not checked on them, the token is
    @Test
    fun aLinkOfNoAddressIsNeverAdmittedByTheCode() {
        val pairing = Pairing()
        assertIs<Pairing.Refusal>(admission(pairing, "site-1", null, pairing.code))
        assertIs<Pairing.Proven>(admission(pairing, "site-1", "10.0.0.2", pairing.code))
        assertIs<Pairing.Proven>(admission(pairing, "site-1", null, pairing.tokenOf("site-1")))
    }

    // as a site requiring no code checks them: nothing counted, no pairing made
    @Test
    fun whatASiteProvedIsKnownWithNoPairing() {
        val pairing = Pairing()
        val nonce = Pairing.newNonce()
        assertEquals(pairing.code, pairing.proven("site-1", nonce, Pairing.proof(pairing.code, "site-1", nonce))?.secret)
        assertNull(pairing.proven("site-1", nonce, Pairing.proof("other", "site-1", nonce)))
        assertTrue(pairing.paired.isEmpty())
    }
}
