package it.unibo.tuprolog.argumentation.parallel.federation

import java.security.SecureRandom

private val random = SecureRandom()

internal actual fun randomBytes(count: Int): ByteArray = ByteArray(count).also { random.nextBytes(it) }
