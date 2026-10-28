package it.unibo.tuprolog.argumentation.parallel.federation

/**
 * The discovery of the sites of a local network, whatever the way their beacons travel (UDP multicast on the JVM and
 * on Node.js): each discovering site announces itself, every [interval] milliseconds, with the port of its
 * WebSocket server ([port]), and [connect]s to the sites it hears of, of its version, that are not its peers yet.
 *
 * Of two sites hearing each other, the one with the lower identifier connects; the other connects as well if no link
 * came after a few beacons (if its own beacons do not reach the first, say). A site that left and comes back is
 * linked to again.
 */
class Beacons(
    private val site: Site,
    private val port: Int,
    private val interval: Long,
    private val connect: (url: String, done: () -> Unit) -> Unit,
) {
    private val lock = Any()
    private val heard = mutableMapOf<String, Long>()
    private val connecting = mutableSetOf<String>()

    /** The beacon of the site, to send every [interval] milliseconds */
    val beacon: String = listOf(PREFIX, Site.VERSION, site.id, port.toString()).joinToString(SEPARATOR)

    /** A beacon [text] from [host], heard at [now] (milliseconds): a link opened to its site if it needs one */
    fun heard(
        text: String,
        host: String,
        now: Long,
    ) {
        val fields = text.split(SEPARATOR)
        if (fields.size != 4 || fields[0] != PREFIX || fields[1] != Site.VERSION) return
        val (_, _, id, port) = fields
        if (id == site.id || id in site.peerSites || port.toIntOrNull() == null) return
        val due =
            locked(lock) {
                val first = heard.getOrPut(id) { now }
                val turn = site.id < id || now - first > PATIENCE * interval
                (turn && connecting.add(id)).also { if (it) heard -= id }
            }
        if (due) connect("ws://${if (':' in host) "[$host]" else host}:$port") { locked(lock) { connecting -= id } }
    }

    companion object {
        /** The UDP port the beacons are sent to */
        const val PORT: Int = 9430

        /** The multicast group the beacons are sent to, within the local network */
        const val GROUP: String = "239.255.42.99"

        private const val PREFIX = "arg2p"
        private const val SEPARATOR = "|"

        // The beacons the site with the higher identifier waits for a link before opening one itself
        private const val PATIENCE = 3
    }
}
