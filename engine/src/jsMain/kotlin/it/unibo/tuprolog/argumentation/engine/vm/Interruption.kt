package it.unibo.tuprolog.argumentation.engine.vm

import it.unibo.tuprolog.argumentation.engine.InternalArg2pApi

private val atomics: dynamic get() = js("Atomics")

// The stops told to this thread (a worker) by another, which counts them on shared memory: the Int32Array and the slot of
// the count, and how many of them the thread took up (see watchStops)
private var stops: dynamic = null
private var stopsSlot = 0
private var stopsTakenUp = 0

/**
 * The stops told to this thread (a worker, which handles no message while it resolves) counted at [slot] of [counter], an
 * Int32Array over a SharedArrayBuffer another thread raises (Atomics.add), from 0, each followed by a message of the stop:
 * until this thread reads it (see [stopTakenUp]) its resolutions halt at their next step, as one on a thread of the JVM does
 * when the thread is interrupted, those of the tasks it was told before the stop too. (A stop counted before this thread
 * watches the count is one of them: its message is still to read.)
 */
@InternalArg2pApi
fun watchStops(
    counter: dynamic,
    slot: Int,
) {
    stops = counter
    stopsSlot = slot
    stopsTakenUp = 0
}

/** The message of a stop read: what this thread is told after it runs, unless another stop comes */
@InternalArg2pApi
fun stopTakenUp() {
    if (stops != null) stopsTakenUp++
}

internal actual fun resolutionInterrupted(): Boolean {
    val counter = stops ?: return false
    return atomics.load(counter, stopsSlot) as Int != stopsTakenUp
}
