package it.unibo.tuprolog.argumentation.parallel.federation

import java.net.Inet4Address
import java.net.InetAddress
import java.net.InetSocketAddress
import java.net.NetworkInterface
import java.net.StandardProtocolFamily
import java.net.StandardSocketOptions
import java.nio.ByteBuffer
import java.nio.channels.ClosedChannelException
import java.nio.channels.DatagramChannel
import java.util.concurrent.Executors
import java.util.concurrent.ThreadFactory
import java.util.concurrent.TimeUnit

/**
 * The sites of the local network found and linked to [site] (see [Beacons]): its beacons sent over UDP multicast,
 * to [group] on [beaconPort], every [interval] milliseconds, with the port of the WebSocket server of [links], [wsPort].
 * The group is joined, and the beacons sent, on every interface able to (the loopback one as well, for the sites of
 * the same machine), over IPv4 alone: a socket of both IPv4 and IPv6 may not reach an IPv4 group (on macOS).
 *
 * The beacons are heard on a thread of their own, and sent on another; the links to the sites heard are opened on
 * threads of their own, as one may wait for a while. A beacon whose handling fails (a callback of the site raising,
 * say) is reported and skipped.
 */
class LanDiscovery(
    site: Site,
    private val links: WebSocketLinks,
    wsPort: Int,
    private val beaconPort: Int = Beacons.PORT,
    group: String = Beacons.GROUP,
    private val interval: Long = 2_000,
) : AutoCloseable {
    private val address = InetAddress.getByName(group)
    private val executor = Executors.newScheduledThreadPool(2, daemons("arg2p-discovery"))
    private val connecting = Executors.newCachedThreadPool(daemons("arg2p-discovery-link"))

    // The addresses of this machine
    private val local =
        NetworkInterface
            .networkInterfaces()
            .toList()
            .flatMap { runCatching { it.inetAddresses().toList() }.getOrDefault(emptyList()) }
            .map { it.hostAddress }
            .toSet()

    // Whether [host] is this machine, whose beacons reach it from each of its addresses
    private fun isLocal(host: String) = host in local || runCatching { InetAddress.getByName(host).isLoopbackAddress }.getOrDefault(false)

    // a site gone since it was heard is no failure: heard again, it is linked to again
    private val beacons =
        Beacons(site, wsPort, interval, ::isLocal) { url, code, done ->
            connecting.execute { runCatching { links.connect(url, code = code) }.also { done() } }
        }

    private val channel =
        DatagramChannel.open(StandardProtocolFamily.INET).apply {
            setOption(StandardSocketOptions.SO_REUSEADDR, true)
            bind(InetSocketAddress(beaconPort))
            setOption(StandardSocketOptions.IP_MULTICAST_TTL, 1)
            setOption(StandardSocketOptions.IP_MULTICAST_LOOP, true)
        }

    // The interfaces up, with an IPv4 address, that joined the group
    private val interfaces =
        NetworkInterface
            .networkInterfaces()
            .toList()
            .filter {
                runCatching {
                    it.isUp &&
                        it.supportsMulticast() &&
                        it.inetAddresses().anyMatch { a ->
                            a is Inet4Address
                        }
                }.getOrDefault(false)
            }.filter { runCatching { channel.join(address, it) }.isSuccess }

    init {
        executor.execute(::listen)
        executor.scheduleAtFixedRate(::announce, 0, interval, TimeUnit.MILLISECONDS)
    }

    /** Stops announcing the site and finding the others: the links opened stay */
    override fun close() {
        executor.shutdownNow()
        connecting.shutdownNow()
        channel.close()
    }

    // the beacon sent on every interface: one failing (down since, say) skipped
    private fun announce() =
        handled {
            val bytes = beacons.beacon.encodeToByteArray()
            val target = InetSocketAddress(address, beaconPort)
            interfaces.forEach { networkInterface ->
                runCatching {
                    channel.setOption(StandardSocketOptions.IP_MULTICAST_IF, networkInterface)
                    channel.send(ByteBuffer.wrap(bytes), target)
                }
            }
        }

    private fun listen() {
        val buffer = ByteBuffer.allocate(1024)
        while (channel.isOpen) {
            buffer.clear()
            val sender =
                try {
                    channel.receive(buffer) as InetSocketAddress
                } catch (_: ClosedChannelException) {
                    return
                }
            buffer.flip()
            val text = ByteArray(buffer.remaining()).also { buffer.get(it) }.decodeToString()
            handled { beacons.heard(text, sender.address.hostAddress, System.currentTimeMillis()) }
        }
    }
}

// The threads of an executor named [name], daemons: they do not keep the JVM running
private fun daemons(name: String) = ThreadFactory { task -> Thread(task, name).apply { isDaemon = true } }
