package it.unibo.tuprolog.argumentation.parallel.federation

import java.net.DatagramPacket
import java.net.InetAddress
import java.net.InetSocketAddress
import java.net.MulticastSocket
import java.net.SocketException
import java.util.concurrent.Executors
import java.util.concurrent.TimeUnit

/**
 * The sites of the local network found and linked to [site] (see [Beacons]): its beacons sent over UDP multicast,
 * to [group] on [beaconPort], every [interval] milliseconds, with the port of the WebSocket server of [links], [wsPort]
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
        Beacons(site, wsPort, interval) { url, done ->
            executor.execute { runCatching { links.connect(url) }.also { done() } }
        }

    @Suppress("DEPRECATION") // joinGroup(InetAddress): the default interface, as the other one needs it named
    private val socket =
        MulticastSocket(null).apply {
            reuseAddress = true
            bind(InetSocketAddress(beaconPort))
            timeToLive = 1
            joinGroup(address)
        }

    init {
        executor.execute(::listen)
        executor.scheduleAtFixedRate(::announce, 0, interval, TimeUnit.MILLISECONDS)
    }

    /** Stops announcing the site and finding the others: the links opened stay */
    override fun close() {
        executor.shutdownNow()
        socket.close()
    }

    private fun announce() {
        val bytes = beacons.beacon.encodeToByteArray()
        runCatching { socket.send(DatagramPacket(bytes, bytes.size, address, beaconPort)) }
    }

    private fun listen() {
        val buffer = ByteArray(1024)
        while (!socket.isClosed) {
            val packet = DatagramPacket(buffer, buffer.size)
            try {
                socket.receive(packet)
            } catch (_: SocketException) {
                return
            }
            val text = packet.data.decodeToString(packet.offset, packet.offset + packet.length)
            beacons.heard(text, packet.address.hostAddress, System.currentTimeMillis())
        }
    }
}
