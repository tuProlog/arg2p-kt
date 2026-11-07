package it.unibo.tuprolog.argumentation.core.engine.vm

internal actual fun <T> locked(
    lock: Any,
    block: () -> T,
): T = block()

internal actual class Waiting actual constructor() {
    actual fun <T> locked(block: () -> T): T = block()

    actual fun await(): Unit = throw IllegalStateException("No other thread to wait for")

    actual fun wakeAll() = Unit
}
