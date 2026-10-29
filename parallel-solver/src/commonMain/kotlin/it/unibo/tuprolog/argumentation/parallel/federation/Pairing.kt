package it.unibo.tuprolog.argumentation.parallel.federation

import kotlin.concurrent.Volatile
import kotlin.time.Duration
import kotlin.time.Duration.Companion.seconds
import kotlin.time.TimeMark
import kotlin.time.TimeSource

/**
 * The code of a site, which the sites linking to it present when it requires one, and the pairings made with it: a
 * site that presented the code is given a token of its own, which it presents instead from then on (its identifier is
 * known to anyone hearing its beacons, the token to the two sites alone). The wrong codes of an address are counted:
 * after a few, its codes are refused for a while, longer and longer, as four digits are soon guessed otherwise.
 */
internal class Pairing {
    private val lock = Any()

    // The wrong codes of an address, and until when its codes are refused
    private class Attempts(
        val wrong: Int,
        val refusedUntil: TimeMark?,
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

    /**
     * Why [site], linking from [address] and presenting [presented] (a code, or the token of a pairing), is refused:
     * null if it is admitted (paired, if it presented the code)
     */
    fun refusal(
        site: String,
        address: String,
        presented: String?,
    ): String? =
        locked(lock) {
            val before = attempts[address]
            when {
                presented != null && tokens[site]?.let { same(presented, it) } == true -> null
                before?.refusedUntil?.hasNotPassedNow() == true -> TOO_MANY
                presented != null && same(presented, code) -> {
                    attempts -= address
                    tokens = tokens + (site to newToken())
                    null
                }
                else -> {
                    val wrong = (before?.wrong ?: 0) + 1
                    attempts[address] = Attempts(wrong, if (wrong < FREE) null else TimeSource.Monotonic.markNow() + lockout(wrong))
                    WRONG
                }
            }
        }

    private companion object {
        // The wrong codes of an address before its codes are refused for a while, and the longest while
        const val FREE = 3
        val LONGEST = 900.seconds

        const val WRONG = "Wrong code"
        const val TOO_MANY = "Too many wrong codes: try again later"

        // A second after the first wrong codes allowed, twice as long after each one more
        fun lockout(wrong: Int): Duration = minOf(LONGEST, 1.seconds * (1 shl minOf(wrong - FREE, 10)))

        // Whether [a] and [b] are the same text, in a time that tells nothing of where they differ
        fun same(
            a: String,
            b: String,
        ): Boolean {
            var difference = a.length xor b.length
            for (i in 0 until maxOf(a.length, b.length)) {
                difference = difference or (a.getOrElse(i) { ' ' }.code xor b.getOrElse(i) { ' ' }.code)
            }
            return difference == 0
        }

        fun newCode(): String =
            randomBytes(4)
                .fold(0L) { acc, byte -> acc * 256 + (byte.toInt() and 0xff) }
                .mod(10_000L)
                .toString()
                .padStart(4, '0')

        fun newToken(): String = randomBytes(16).joinToString("") { (it.toInt() and 0xff).toString(16).padStart(2, '0') }
    }
}

/** [count] random bytes, from the random source of the platform fit for secrets */
internal expect fun randomBytes(count: Int): ByteArray
