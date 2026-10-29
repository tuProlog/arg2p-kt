package it.unibo.tuprolog.argumentation.parallel.federation

/**
 * The HMAC-SHA-256 of [message] with [key] (RFC 2104, FIPS 180-4), in hexadecimal: written here, as the platforms
 * share no synchronous one (the Web Crypto of browsers is asynchronous)
 */
internal fun hmac(
    key: String,
    message: String,
): String {
    val keyBytes = key.encodeToByteArray().let { if (it.size > BLOCK) sha256(it) else it }.copyOf(BLOCK)
    val inner = sha256(ByteArray(BLOCK) { (keyBytes[it].toInt() xor 0x36).toByte() } + message.encodeToByteArray())
    val outer = sha256(ByteArray(BLOCK) { (keyBytes[it].toInt() xor 0x5c).toByte() } + inner)
    return outer.joinToString("") { (it.toInt() and 0xff).toString(16).padStart(2, '0') }
}

// The bytes of a block of SHA-256
private const val BLOCK = 64

// The first 32 bits of the fractional parts of the cube roots of the first 64 primes
private val ROUNDS =
    longArrayOf(
        0x428a2f98,
        0x71374491,
        0xb5c0fbcf,
        0xe9b5dba5,
        0x3956c25b,
        0x59f111f1,
        0x923f82a4,
        0xab1c5ed5,
        0xd807aa98,
        0x12835b01,
        0x243185be,
        0x550c7dc3,
        0x72be5d74,
        0x80deb1fe,
        0x9bdc06a7,
        0xc19bf174,
        0xe49b69c1,
        0xefbe4786,
        0x0fc19dc6,
        0x240ca1cc,
        0x2de92c6f,
        0x4a7484aa,
        0x5cb0a9dc,
        0x76f988da,
        0x983e5152,
        0xa831c66d,
        0xb00327c8,
        0xbf597fc7,
        0xc6e00bf3,
        0xd5a79147,
        0x06ca6351,
        0x14292967,
        0x27b70a85,
        0x2e1b2138,
        0x4d2c6dfc,
        0x53380d13,
        0x650a7354,
        0x766a0abb,
        0x81c2c92e,
        0x92722c85,
        0xa2bfe8a1,
        0xa81a664b,
        0xc24b8b70,
        0xc76c51a3,
        0xd192e819,
        0xd6990624,
        0xf40e3585,
        0x106aa070,
        0x19a4c116,
        0x1e376c08,
        0x2748774c,
        0x34b0bcb5,
        0x391c0cb3,
        0x4ed8aa4a,
        0x5b9cca4f,
        0x682e6ff3,
        0x748f82ee,
        0x78a5636f,
        0x84c87814,
        0x8cc70208,
        0x90befffa,
        0xa4506ceb,
        0xbef9a3f7,
        0xc67178f2,
    ).map { it.toInt() }.toIntArray()

// The SHA-256 digest of [data]
private fun sha256(data: ByteArray): ByteArray {
    val hash =
        longArrayOf(
            0x6a09e667,
            0xbb67ae85,
            0x3c6ef372,
            0xa54ff53a,
            0x510e527f,
            0x9b05688c,
            0x1f83d9ab,
            0x5be0cd19,
        ).map { it.toInt() }.toIntArray()
    // the data, a 1 bit, zeros, and its length in bits, to a whole number of blocks
    val blocks = (data.size + 9 + BLOCK - 1) / BLOCK
    val padded = data.copyOf(blocks * BLOCK)
    padded[data.size] = 0x80.toByte()
    val bits = data.size.toLong() * 8
    for (i in 0 until 8) padded[padded.size - 1 - i] = (bits ushr (8 * i)).toByte()
    val words = IntArray(64)
    for (block in 0 until blocks) {
        for (t in 0 until 16) {
            val at = block * BLOCK + t * 4
            words[t] =
                ((padded[at].toInt() and 0xff) shl 24) or ((padded[at + 1].toInt() and 0xff) shl 16) or
                ((padded[at + 2].toInt() and 0xff) shl 8) or (padded[at + 3].toInt() and 0xff)
        }
        for (t in 16 until 64) {
            val s0 = words[t - 15].rotateRight(7) xor words[t - 15].rotateRight(18) xor (words[t - 15] ushr 3)
            val s1 = words[t - 2].rotateRight(17) xor words[t - 2].rotateRight(19) xor (words[t - 2] ushr 10)
            words[t] = words[t - 16] + s0 + words[t - 7] + s1
        }
        var a = hash[0]
        var b = hash[1]
        var c = hash[2]
        var d = hash[3]
        var e = hash[4]
        var f = hash[5]
        var g = hash[6]
        var h = hash[7]
        for (t in 0 until 64) {
            val t1 =
                h + (e.rotateRight(6) xor e.rotateRight(11) xor e.rotateRight(25)) + ((e and f) xor (e.inv() and g)) + ROUNDS[t] + words[t]
            val t2 = (a.rotateRight(2) xor a.rotateRight(13) xor a.rotateRight(22)) + ((a and b) xor (a and c) xor (b and c))
            h = g
            g = f
            f = e
            e = d + t1
            d = c
            c = b
            b = a
            a = t1 + t2
        }
        hash[0] += a
        hash[1] += b
        hash[2] += c
        hash[3] += d
        hash[4] += e
        hash[5] += f
        hash[6] += g
        hash[7] += h
    }
    return ByteArray(32) { (hash[it / 4] ushr (24 - 8 * (it % 4))).toByte() }
}
