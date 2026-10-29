package it.unibo.tuprolog.argumentation.parallel.federation

/**
 * The discovery of the sites of a local network, whatever the way their beacons travel (UDP multicast on the JVM and
 * on Node.js): each discovering site announces itself, every [interval] milliseconds, with its name, the port of its
 * WebSocket server ([port]) and whether it requires a code (see [Site.requiresCode]); the sites it hears, of its
 * version, are its nearby ones (see [Site.nearby]), until they are heard no more.
 *
 * A site [connect]s to the sites it hears that are not its peers yet, unless one of the two requires a code: those it
 * is paired with ([Site.pairedSites]) alone then, presenting the token of their pairing, the others waiting to be
 * paired by hand. Of two sites hearing each other, the one with the lower identifier connects; the other connects as
 * well if no link came after a few beacons (if its own beacons do not reach the first, say). A site that left and
 * comes back is linked to again.
 */
class Beacons(
    private val site: Site,
    private val port: Int,
    private val interval: Long,
    private val connect: (url: String, code: String?, done: () -> Unit) -> Unit,
) {
    /** The discovery of [site], connecting with no code */
    constructor(
        site: Site,
        port: Int,
        interval: Long,
        connect: (url: String, done: () -> Unit) -> Unit,
    ) : this(site, port, interval, { url, _, done -> connect(url, done) })

    private val lock = Any()
    private val heard = mutableMapOf<String, Long>()
    private val connecting = mutableSetOf<String>()

    // The sites heard, with when they were last
    private val nearby = mutableMapOf<String, Pair<NearbySite, Long>>()

    /** The beacon of the site, to send every [interval] milliseconds */
    val beacon: String
        get() =
            listOf(PREFIX, Site.VERSION, site.id, port.toString(), if (site.requiresCode) "1" else "0", site.name)
                .joinToString(SEPARATOR)

    /** A beacon [text] from [host], heard at [now] (milliseconds): a site nearby, and a link opened to it if it needs one */
    fun heard(
        text: String,
        host: String,
        now: Long,
    ) {
        val fields = text.split(SEPARATOR, limit = FIELDS)
        val valid = fields.size == FIELDS && fields[0] == PREFIX && fields[1] == Site.VERSION && fields[3].toIntOrNull() != null
        val id = fields.getOrNull(2)
        val url = "ws://${if (':' in host) "[$host]" else host}:${fields.getOrNull(3)}"
        val gated = fields.getOrNull(4) == "1"
        site.heard(
            locked(lock) {
                if (valid && id != null && id != site.id) nearby[id] = NearbySite(id, fields[5], url, gated) to now
                // the sites not heard for a few beacons gone (this one hears its own beacons, if no other)
                nearby.entries.removeAll { now - it.value.second > PATIENCE * interval }
                nearby.values.map { it.first }.sortedBy { it.id }
            },
        )
        if (!valid || id == null || id == site.id || id in site.peerSites) return
        val token = site.tokenOf(id)
        if (token == null && (gated || site.requiresCode)) return
        val due =
            locked(lock) {
                val first = heard.getOrPut(id) { now }
                val turn = site.id < id || now - first > PATIENCE * interval
                (turn && connecting.add(id)).also { if (it) heard -= id }
            }
        if (due) connect(url, token) { locked(lock) { connecting -= id } }
    }

    companion object {
        /** The UDP port the beacons are sent to */
        const val PORT: Int = 9430

        /** The multicast group the beacons are sent to, within the local network */
        const val GROUP: String = "239.255.42.99"

        private const val PREFIX = "arg2p"
        private const val SEPARATOR = "|"

        // The fields of a beacon, the name last (it may hold the separator)
        private const val FIELDS = 6

        // The beacons the site with the higher identifier waits for a link before opening one itself, and those a site
        // nearby may miss before it is gone
        private const val PATIENCE = 3
    }
}
