package it.unibo.tuprolog.argumentation.parallel.federation

/**
 * The discovery of the sites of a local network, whatever the way their beacons travel (UDP multicast on the JVM and
 * on Node.js): each discovering site announces itself, every [interval] milliseconds, with its name, the port of its
 * WebSocket server ([port]) and whether it requires a code (see [Site.requiresCode]); the sites it hears, of its
 * version, are its nearby ones (see [Site.nearby]), until they are heard no more.
 *
 * A site [connect]s to the sites it hears that are not its peers yet, unless one of the two requires a code: those it
 * is paired with ([Site.pairedSites]) alone then, proving it knows the token of their pairing, the others waiting to
 * be paired by hand. Of two sites hearing each other, the one with the lower identifier connects; the other connects
 * as well if no link came after a few beacons (if its own beacons do not reach the first, say). A site that left and
 * comes back is linked to again.
 *
 * Anyone may send a beacon: a site whose identifier is claimed by the beacons of two hosts at once (this one counting
 * as one, by [local], as its own beacons reach it on every interface) is neither nearby nor linked to, until only
 * one of them is heard; a site claiming the identifier of a site paired with this one is never linked to (it cannot
 * prove it knows the token of their pairing, see [Pairing]).
 *
 * The constructor and the beacons are those of the transports of the module; the [port][PORT] and the [group][GROUP]
 * of the beacons are public.
 */
class Beacons internal constructor(
    private val site: Site,
    private val port: Int,
    private val interval: Long,
    private val local: (host: String) -> Boolean,
    private val connect: (url: String, code: String?, done: () -> Unit) -> Unit,
) {
    private val lock = Any()
    private val heard = mutableMapOf<String, Long>()
    private val connecting = mutableSetOf<String>()

    // The sites heard, with when they were last
    private val nearby = mutableMapOf<String, Pair<NearbySite, Long>>()

    // The hosts the beacons of each site came from lately (this one as LOCAL), with when they last did
    private val hosts = mutableMapOf<String, MutableMap<String, Long>>()

    /** The beacon of the site, to send every [interval] milliseconds */
    internal val beacon: String
        get() =
            listOf(PREFIX, Site.VERSION, site.id, port.toString(), if (site.requiresCode) "1" else "0", site.name)
                .joinToString(SEPARATOR)

    /** A beacon [text] from [host], heard at [now] (milliseconds): a site nearby, and a link opened to it if it needs one */
    internal fun heard(
        text: String,
        host: String,
        now: Long,
    ) {
        val fields = text.split(SEPARATOR, limit = FIELDS)
        val valid = fields.size == FIELDS && fields[0] == PREFIX && fields[1] == Site.VERSION && fields[3].toIntOrNull() != null
        val id = fields.getOrNull(2)?.takeIf { valid && it != site.id }
        val url = "ws://${if (':' in host) "[$host]" else host}:${fields.getOrNull(3)}"
        val gated = fields.getOrNull(4) == "1"
        val forgotten = now - PATIENCE * interval
        val (sites, claimed) =
            locked(lock) {
                id?.let { hosts.getOrPut(it) { mutableMapOf() }[if (local(host)) LOCAL else host] = now }
                hosts.values.forEach { it.values.removeAll { last -> last < forgotten } }
                hosts.values.removeAll { it.isEmpty() }
                if (id != null) nearby[id] = NearbySite(id, fields[5], url, gated) to now
                // the sites not heard for a few beacons gone (this one hears its own beacons, if no other)
                nearby.entries.removeAll { it.value.second < forgotten }
                val contested = hosts.filterValues { it.size > 1 }.keys
                val listed = nearby.filterKeys { it !in contested }.values.map { it.first }
                listed.sortedBy { it.id } to id?.takeIf { it !in contested }
            }
        site.heard(sites)
        if (claimed == null) return
        if (claimed in site.peerSites) {
            // linked: when it was first heard counts again from its next beacons, once the link closes
            locked(lock) { heard -= claimed }
            return
        }
        val token = site.tokenOf(claimed)
        if (token == null && (gated || site.requiresCode)) return
        val due =
            locked(lock) {
                val first = heard.getOrPut(claimed) { now }
                val turn = site.id < claimed || now - first > PATIENCE * interval
                (turn && connecting.add(claimed)).also { if (it) heard -= claimed }
            }
        if (due) connect(url, token) { locked(lock) { connecting -= claimed } }
    }

    companion object {
        /** The UDP port the beacons are sent to */
        const val PORT: Int = 9430

        /** The multicast group the beacons are sent to, within the local network */
        const val GROUP: String = "239.255.42.99"

        private const val PREFIX = "arg2p"
        private const val SEPARATOR = "|"

        // The host of this site, among those the beacons come from
        private const val LOCAL = ""

        // The fields of a beacon, the name last (it may hold the separator)
        private const val FIELDS = 6

        // The beacons the site with the higher identifier waits for a link before opening one itself, and those a site
        // nearby may miss before it is gone
        private const val PATIENCE = 3
    }
}
