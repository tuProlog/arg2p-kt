package it.unibo.tuprolog.argumentation.parallel.federation

import org.khronos.webgl.Int8Array

// The Web Crypto of the runtime (browsers, Node.js)
internal actual fun randomBytes(count: Int): ByteArray {
    val bytes = Int8Array(count)
    js("globalThis.crypto.getRandomValues(bytes)")
    return ByteArray(count) { bytes.asDynamic()[it] as Byte }
}
