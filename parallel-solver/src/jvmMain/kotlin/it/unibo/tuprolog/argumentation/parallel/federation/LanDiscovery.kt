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
import java.util.concurrent.TimeUnit

/**
 * The sites of the local network found and linked to [site] (see [Beacons]): its beacons sent over UDP multicast,
 * to [group] on [beaconPort], every [interval] milliseconds, with the port of the WebSocket server of [links], [wsPort].
 * The group is joined, and the beacons sent, on every interface able to (the loopback one as well, for the sites of
 * the same machine), over IPv4 alone: a socket of both IPv4 and IPv6 may not reach an IPv4 group (on macOS).
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
    private val executor =
        Executors.newScheduledThreadPool(2) { task -> Thread(task, "arg2p-discovery").apply { isDaemon = true } }
    private val beacons =
        Beacons(site, wsPort, interval) { url, code, done ->
            executor.execute { runCatching { links.connect(url, code = code) }.also { done() } }
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
        channel.close()
    }

    private fun announce() {
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
            beacons.heard(text, sender.address.hostAddress, System.currentTimeMillis())
        }
    }
}
