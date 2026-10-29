package it.unibo.tuprolog.argumentation.parallel.federation

import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec
import kotlin.random.Random
import kotlin.test.Test
import kotlin.test.assertEquals

// The HMAC-SHA-256 of the proofs of the sites, as the JDK computes it
class HmacTest {
    @Test
    fun theHmacIsTheOneOfTheJdk() {
        val random = Random(42)
        // keys and messages of every length about the blocks of SHA-256 (64 bytes), and longer
        (0..140).forEach { length ->
            val key = (1..random.nextInt(1, 100)).map { 'a' + random.nextInt(26) }.joinToString("")
            val message = (1..length).map { (' ' + random.nextInt(95)) }.joinToString("")
            assertEquals(jdk(key, message), hmac(key, message), "$key / $message")
        }
        // RFC 4231, test case 2
        assertEquals("5bdcc146bf60754e6a042426089575c75a003f089d2739839dec58b964ec3843", hmac("Jefe", "what do ya want for nothing?"))
    }

    private fun jdk(
        key: String,
        message: String,
    ): String =
        Mac
            .getInstance("HmacSHA256")
            .apply { init(SecretKeySpec(key.encodeToByteArray(), "HmacSHA256")) }
            .doFinal(message.encodeToByteArray())
            .joinToString("") { "%02x".format(it) }
}
