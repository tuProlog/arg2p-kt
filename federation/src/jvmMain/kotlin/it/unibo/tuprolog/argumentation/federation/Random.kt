package it.unibo.tuprolog.argumentation.federation

import java.security.SecureRandom

private val random = SecureRandom()

internal actual fun randomBytes(count: Int): ByteArray = ByteArray(count).also { random.nextBytes(it) }
