package it.unibo.tuprolog.argumentation.federation

import java.io.ByteArrayOutputStream
import java.math.BigInteger
import java.net.InetAddress
import java.security.KeyPairGenerator
import java.security.KeyStore
import java.security.SecureRandom
import java.security.Signature
import java.security.cert.CertificateFactory
import java.security.cert.X509Certificate
import java.time.ZoneOffset
import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter
import javax.net.ssl.KeyManagerFactory
import javax.net.ssl.SSLContext

/**
 * A certificate signed by its own key, made as it is needed, for a page of the local network: a browser warns about it
 * once, and the page is then a secure context (what SharedArrayBuffer needs, see WebWorkerSolver). The JDK has no API to
 * make one: it is written here, in DER, as X.509 lays it out.
 */
object SelfSigned {
    // How long a certificate lasts, in days, from the day before it was made (the clocks of a phone and of this host may differ)
    private const val DAYS = 365L

    /** A context for a server known by [addresses], names or IP addresses, with a new key and certificate */
    fun context(addresses: List<String>): SSLContext {
        val keys = KeyPairGenerator.getInstance("RSA").apply { initialize(2048) }.generateKeyPair()
        val tbs = toBeSigned(addresses, keys.public.encoded)
        val signature =
            Signature.getInstance("SHA256withRSA").run {
                initSign(keys.private)
                update(tbs)
                sign()
            }
        val der = sequence(tbs, algorithm, tagged(BIT_STRING, byteArrayOf(0) + signature))
        val certificate = CertificateFactory.getInstance("X.509").generateCertificate(der.inputStream()) as X509Certificate
        val password = CharArray(0)
        val store = KeyStore.getInstance("PKCS12").apply { load(null, password) }
        store.setKeyEntry("site", keys.private, password, arrayOf(certificate))
        val managers = KeyManagerFactory.getInstance(KeyManagerFactory.getDefaultAlgorithm()).apply { init(store, password) }
        return SSLContext.getInstance("TLS").apply { init(managers.keyManagers, null, SecureRandom()) }
    }

    private fun toBeSigned(
        addresses: List<String>,
        publicKey: ByteArray,
    ): ByteArray {
        val name = sequence(tagged(SET, sequence(oid(2, 5, 4, 3), tagged(UTF8_STRING, "arg2p".encodeToByteArray()))))
        val now = ZonedDateTime.now(ZoneOffset.UTC)
        val serial = BigInteger(64, SecureRandom()).setBit(62).toByteArray()
        return sequence(
            tagged(EXPLICIT_0, tagged(INTEGER, byteArrayOf(2))),
            tagged(INTEGER, serial),
            algorithm,
            name,
            sequence(time(now.minusDays(1)), time(now.plusDays(DAYS))),
            name,
            publicKey,
            tagged(EXPLICIT_3, sequence(sequence(oid(2, 5, 29, 17), tagged(OCTET_STRING, alternativeNames(addresses))))),
        )
    }

    // Each address as the certificate says it: an IP address by its bytes, a name by its letters
    private fun alternativeNames(addresses: List<String>): ByteArray =
        sequence(
            *addresses
                .map {
                    if (it.firstOrNull()?.isDigit() == true) {
                        tagged(IP_ADDRESS, InetAddress.getByName(it).address)
                    } else {
                        tagged(DNS_NAME, it.encodeToByteArray())
                    }
                }.toTypedArray(),
        )

    private val algorithm = sequence(oid(1, 2, 840, 113549, 1, 1, 11), byteArrayOf(NULL, 0))

    private fun time(at: ZonedDateTime): ByteArray =
        tagged(UTC_TIME, at.format(DateTimeFormatter.ofPattern("yyMMddHHmmss'Z'")).encodeToByteArray())

    private fun sequence(vararg parts: ByteArray): ByteArray = tagged(SEQUENCE, parts.reduce(ByteArray::plus))

    private fun oid(vararg arcs: Long): ByteArray {
        val out = ByteArrayOutputStream()
        out.write((arcs[0] * 40 + arcs[1]).toInt())
        for (arc in arcs.drop(2)) {
            var shift = (63 - arc.countLeadingZeroBits()) / 7 * 7
            while (shift > 0) {
                out.write((((arc shr shift) and 0x7f) or 0x80).toInt())
                shift -= 7
            }
            out.write((arc and 0x7f).toInt())
        }
        return tagged(OID, out.toByteArray())
    }

    private fun oid(
        a: Int,
        b: Int,
        vararg arcs: Int,
    ): ByteArray = oid(a.toLong(), b.toLong(), *arcs.map { it.toLong() }.toLongArray())

    // A value with its tag and its length, in as few bytes as it takes
    private fun tagged(
        tag: Byte,
        value: ByteArray,
    ): ByteArray {
        val size = value.size
        val length =
            if (size < 0x80) {
                byteArrayOf(size.toByte())
            } else {
                val bytes =
                    BigInteger
                        .valueOf(size.toLong())
                        .toByteArray()
                        .dropWhile { it == 0.toByte() }
                        .toByteArray()
                byteArrayOf((0x80 or bytes.size).toByte()) + bytes
            }
        return byteArrayOf(tag) + length + value
    }

    private const val INTEGER: Byte = 0x02
    private const val BIT_STRING: Byte = 0x03
    private const val OCTET_STRING: Byte = 0x04
    private const val NULL: Byte = 0x05
    private const val OID: Byte = 0x06
    private const val UTF8_STRING: Byte = 0x0c
    private const val UTC_TIME: Byte = 0x17
    private const val SEQUENCE: Byte = 0x30
    private const val SET: Byte = 0x31
    private const val DNS_NAME: Byte = 0x82.toByte()
    private const val IP_ADDRESS: Byte = 0x87.toByte()
    private const val EXPLICIT_0: Byte = 0xa0.toByte()
    private const val EXPLICIT_3: Byte = 0xa3.toByte()
}
