package it.unibo.tuprolog.argumentation.parallel.host

import it.unibo.tuprolog.argumentation.engine.vm.watchStops
import it.unibo.tuprolog.argumentation.engine.vm.stopTakenUp as takeUpStop

/**
 * The frame a thread waiting on its inbox sends its node once it read frames the node could not write for lack of room
 * (see [SharedOutbox])
 */
internal const val DRAINED = "\u0000drained"

// The units of text a channel holds: its frames wait in its writer beyond them
private const val UNITS = 1 shl 20

// The slots of the control of a channel: the units written, the units read (both counted from the start, wrapping
// around), whether the writer holds frames it could not write, and the stops the writer told the reader (see watchStops)
private const val WRITTEN = 0
private const val READ = 1
private const val WAITING = 2
private const val STOPS = 3

/** A channel for the frames of a thread that waits: a SharedArrayBuffer, its control then its units of text */
internal fun newChannel(): dynamic {
    val bytes = 16 + UNITS * 2
    return js("new SharedArrayBuffer(bytes)")
}

private fun controlOf(channel: dynamic): dynamic = js("new Int32Array(channel, 0, 4)")

private fun unitsOf(channel: dynamic): dynamic = js("new Uint16Array(channel, 16)")

private val atomics: dynamic get() = js("Atomics")

private fun index(
    position: Int,
    size: Int,
): Int = ((position % size) + size) % size

/**
 * The inbox of a thread that waits on [channel] (a worker of JavaScript, which handles no message while it waits there):
 * each frame its length then its units, read as they come, [drained] told once the writer waits for room. The resolutions
 * of the thread halt at the stops the writer tells (see [SharedOutbox.interrupt]): a worker resolving reads no frame.
 */
internal class SharedInbox(
    channel: dynamic,
    private val drained: () -> Unit,
) : Inbox {
    private val control: dynamic = controlOf(channel)
    private val units: dynamic = unitsOf(channel)
    private val size: Int = units.length as Int

    init {
        watchStops(control, STOPS)
    }

    override fun stopTakenUp() = takeUpStop()

    override fun take(): String {
        val header = read(2)
        val length = (header[0].code shl 16) or header[1].code
        return read(length).concatToString()
    }

    private fun read(count: Int): CharArray {
        val found = CharArray(count)
        var got = 0
        while (got < count) {
            val written = atomics.load(control, WRITTEN) as Int
            val read = atomics.load(control, READ) as Int
            val available = written - read
            if (available == 0) {
                signal()
                atomics.wait(control, WRITTEN, written)
                continue
            }
            val taken = minOf(available, count - got)
            for (i in 0 until taken) found[got + i] = (units[index(read + i, size)] as Int).toChar()
            got += taken
            atomics.store(control, READ, read + taken)
            signal()
        }
        return found
    }

    // (the writer told, once, that there is room again)
    private fun signal() {
        if (atomics.compareExchange(control, WAITING, 1, 0) as Int == 1) drained()
    }
}

/**
 * The writer of [channel], on the thread of the node, which never waits: the frames that do not fit wait here, written
 * as the reader makes room ([flush], once it tells so, see [DRAINED])
 */
internal class SharedOutbox(
    channel: dynamic,
) : HostedThread {
    private val control: dynamic = controlOf(channel)
    private val units: dynamic = unitsOf(channel)
    private val size: Int = units.length as Int
    private val frames = ArrayDeque<String>()

    // the units of the first frame waiting written already, its two of length first
    private var done = 0

    override fun put(frame: String) {
        frames.addLast(frame)
        flush()
    }

    // (read by the reader's resolution at its next check, see watchStops)
    override fun interrupt() {
        atomics.add(control, STOPS, 1)
    }

    /** The frames waiting written, as many as fit */
    fun flush() {
        while (frames.isNotEmpty()) {
            val frame = frames.first()
            val total = frame.length + 2
            var position = atomics.load(control, WRITTEN) as Int
            var room = size - (position - atomics.load(control, READ) as Int)
            while (room > 0 && done < total) {
                units[index(position, size)] =
                    when (done) {
                        0 -> frame.length ushr 16
                        1 -> frame.length and 0xffff
                        else -> frame[done - 2].code
                    }
                position++
                done++
                room--
            }
            atomics.store(control, WRITTEN, position)
            atomics.notify(control, WRITTEN)
            if (done == total) {
                frames.removeFirst()
                done = 0
                continue
            }
            // (told by the reader once it reads: unless it read meanwhile, before seeing the flag)
            atomics.store(control, WAITING, 1)
            if (size - (position - atomics.load(control, READ) as Int) == 0) return
        }
    }
}
