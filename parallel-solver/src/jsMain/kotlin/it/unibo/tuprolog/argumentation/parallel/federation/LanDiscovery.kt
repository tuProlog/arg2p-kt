package it.unibo.tuprolog.argumentation.parallel.federation

import kotlin.js.Promise

/**
 * The sites of the local network found and linked to [site] (see [Beacons]), on Node.js: its beacons sent over UDP
 * multicast, to [group] on [beaconPort], every [interval] milliseconds, with the port of the WebSocket server of
 * [links], [wsPort]. Neither the beacons nor their socket keep Node.js running.
 */
class LanDiscovery(
    site: Site,
    private val links: WebSocketLinks,
    wsPort: Int,
    private val beaconPort: Int = Beacons.PORT,
    private val group: String = Beacons.GROUP,
    private val interval: Int = 2_000,
) {
    private val beacons =
        Beacons(site, wsPort, interval.toLong()) { url, code, done ->
            links.connect(url, code).then({ done() }, { done() })
        }
    private var socket: dynamic = null
    private var timer: dynamic = null

    /** Starts announcing the site and finding the others */
    fun start(): Promise<Unit> =
        Promise { resolve, reject ->
            val options: dynamic = js("({})")
            options.type = "udp4"
            options.reuseAddr = true
            val opened = nodeModule("dgram").createSocket(options)
            opened.on("error") { error: dynamic -> reject(error as Throwable) }
            opened.on("message") { message: dynamic, remote: dynamic ->
                handled {
                    beacons.heard(
                        message.toString("utf8") as String,
                        remote.address as String,
                        (js("Date.now()") as Double).toLong(),
                    )
                }
            }
            opened.bind(beaconPort) {
                opened.addMembership(group)
                opened.setMulticastTTL(1)
                opened.setMulticastLoopback(true)
                opened.unref()
                socket = opened
                val announce = { opened.send(beacons.beacon, beaconPort, group) }
                announce()
                timer = every(interval, announce)
                timer.unref()
                resolve(Unit)
            }
        }

    /** Stops announcing the site and finding the others: the links opened stay */
    fun close() {
        if (timer != null) stop(timer)
        socket?.close()
        timer = null
        socket = null
    }
}

private fun every(
    millis: Int,
    task: () -> Unit,
): dynamic = js("setInterval(task, millis)")

private fun stop(timer: dynamic) {
    js("clearInterval(timer)")
}
