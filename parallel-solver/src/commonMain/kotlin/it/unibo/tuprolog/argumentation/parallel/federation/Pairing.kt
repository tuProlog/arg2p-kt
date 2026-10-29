package it.unibo.tuprolog.argumentation.parallel.federation

import kotlin.concurrent.Volatile
import kotlin.time.Duration
import kotlin.time.Duration.Companion.hours
import kotlin.time.Duration.Companion.seconds
import kotlin.time.TimeMark
import kotlin.time.TimeSource

/**
 * The code of a site, which the sites linking to it prove they know when it requires one, and the pairings made with
 * it: a site that proved the code is given a token of its own, which it proves it knows instead from then on (its
 * identifier is known to anyone hearing its beacons, the token to the two sites alone), and the code is renewed. A
 * secret is never sent but the token, once, to the site just paired: a site proves it knows one with its [proof] on a
 * nonce of the other, as a site claiming the identifier of another (a beacon of its own) learns nothing of it.
 *
 * The wrong codes of an address are counted: after a few, its codes are refused for a while, longer and longer, as
 * four digits are soon guessed otherwise; a link with no address is never admitted by its code, as its wrong codes
 * could not be counted. The wrong codes of an address are forgotten a while after the last one.
 */
internal class Pairing {
    private val lock = Any()

    // The wrong codes of an address, until when its codes are refused, and when the last one came
    private class Attempts(
        val wrong: Int,
        val refusedUntil: TimeMark?,
        val last: TimeMark,
    )

    private val attempts = mutableMapOf<String, Attempts>()

    /** The code of the site: four digits, random */
    @Volatile var code: String = newCode()
        private set

    // The token of the pairing with each site paired with this one
    @Volatile private var tokens: Map<String, String> = emptyMap()

    /** The sites paired with this one */
    val paired: Set<String> get() = tokens.keys

    /** A new code, the one before refused from then on */
    fun renew(): String = newCode().also { code = it }

    /** The token of the pairing with [site], if they are paired */
    fun tokenOf(site: String): String? = tokens[site]

    /** [site] paired with this one, by [token] */
    fun remember(
        site: String,
        token: String,
    ) = locked(lock) { tokens = tokens + (site to token) }

    /** What a site linking to this one is told: admitted, or refused */
    sealed interface Admission

    /** What a site proved: the [secret] it knows (the token of its pairing, or the code), and the [token] given it if it proved the code */
    class Proven(
        val secret: String,
        val token: String? = null,
    ) : Admission

    /** A site refused, and why */
    class Refusal(
        val reason: String,
    ) : Admission

    /**
     * What [site], linking from [address], proved with [proof] on [nonce] (see [proof]), checked as a site requiring a
     * code checks it: the token of its pairing, or the code, pairing the two then (the code renewed); why it is refused
     * otherwise
     */
    fun admission(
        site: String,
        address: String?,
        nonce: String,
        proof: String?,
    ): Admission =
        locked(lock) {
            val now = TimeSource.Monotonic.markNow()
            attempts.entries.removeAll { (it.value.last + FORGOTTEN).hasPassedNow() }
            val before = address?.let { attempts[it] }
            val paired = tokens[site]
            when {
                paired != null && proves(proof, paired, site, nonce) -> Proven(paired)
                address == null -> Refusal(NO_ADDRESS)
                before?.refusedUntil?.hasNotPassedNow() == true -> Refusal(TOO_MANY)
                proves(proof, code, site, nonce) -> {
                    attempts -= address
                    val token = newToken()
                    tokens = tokens + (site to token)
                    Proven(code, token).also { code = newCode() }
                }
                else -> {
                    val wrong = (before?.wrong ?: 0) + 1
                    attempts[address] = Attempts(wrong, if (wrong < FREE) null else now + lockout(wrong), now)
                    Refusal(WRONG)
                }
            }
        }

    /**
     * What [site] proved with [proof] on [nonce], checked as a site requiring no code checks it (nothing counted, no
     * pairing made): the token of its pairing, or the code; null if neither
     */
    fun proven(
        site: String,
        nonce: String,
        proof: String,
    ): Proven? {
        val token = tokens[site]
        val code = code
        return when {
            token != null && proves(proof, token, site, nonce) -> Proven(token)
            proves(proof, code, site, nonce) -> Proven(code)
            else -> null
        }
    }

    companion object {
        // The wrong codes of an address before its codes are refused for a while, the longest while, and how long
        // after the last one they are forgotten
        private const val FREE = 3
        private val LONGEST = 900.seconds
        private val FORGOTTEN = 1.hours

        private const val WRONG = "Wrong code"
        private const val TOO_MANY = "Too many wrong codes: try again later"
        private const val NO_ADDRESS = "No code is checked on a link of no address"

        /** The proof that [site] knows [secret] (a code, or the token of a pairing), on [nonce]: none of them told */
        fun proof(
            secret: String,
            site: String,
            nonce: String,
        ): String = hmac(secret, "$site|$nonce")

        /** Whether [proof] is the proof that [site] knows [secret] on [nonce] */
        fun proves(
            proof: String?,
            secret: String,
            site: String,
            nonce: String,
        ): Boolean = proof != null && same(proof, proof(secret, site, nonce))

        /** A nonce, for the other end of a link to prove its secret on: random */
        fun newNonce(): String = randomHex(16)

        // A second after the first wrong codes allowed, twice as long after each one more
        private fun lockout(wrong: Int): Duration = minOf(LONGEST, 1.seconds * (1 shl minOf(wrong - FREE, 10)))

        // Whether [a] and [b] are the same text, in a time that tells nothing of where they differ
        private fun same(
            a: String,
            b: String,
        ): Boolean {
            var difference = a.length xor b.length
            for (i in 0 until maxOf(a.length, b.length)) {
                difference = difference or (a.getOrElse(i) { ' ' }.code xor b.getOrElse(i) { ' ' }.code)
            }
            return difference == 0
        }

        private fun newCode(): String =
            randomBytes(4)
                .fold(0L) { acc, byte -> acc * 256 + (byte.toInt() and 0xff) }
                .mod(10_000L)
                .toString()
                .padStart(4, '0')

        private fun newToken(): String = randomHex(16)

        // [count] random bytes, in hexadecimal
        private fun randomHex(count: Int): String =
            randomBytes(count).joinToString("") { (it.toInt() and 0xff).toString(16).padStart(2, '0') }
    }
}

/** [count] random bytes, from the random source of the platform fit for secrets */
internal expect fun randomBytes(count: Int): ByteArray
