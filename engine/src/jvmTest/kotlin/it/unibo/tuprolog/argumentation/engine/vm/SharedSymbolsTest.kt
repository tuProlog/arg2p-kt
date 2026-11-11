package it.unibo.tuprolog.argumentation.engine.vm

import it.unibo.tuprolog.argumentation.engine.words.Symbols
import java.util.concurrent.CountDownLatch
import kotlin.concurrent.thread
import kotlin.test.Test
import kotlin.test.assertEquals

/** A table of symbols a session shares, interned into by threads at once: each symbol once, under one id */
class SharedSymbolsTest {
    @Test
    fun threadsInternEachSymbolOnce() {
        val symbols = Symbols.shared(Symbols.of(null).also { it.atom("before") })
        val threads = 6
        val names = 4000
        val start = CountDownLatch(1)
        val atoms = Array(threads) { IntArray(names) }
        val functors = Array(threads) { IntArray(names) }
        (0 until threads)
            .map { t ->
                thread {
                    start.await()
                    // (each thread in its own order, so that they race on the same names)
                    for (k in 0 until names) {
                        val i = if (t % 2 == 0) k else names - 1 - k
                        atoms[t][i] = symbols.atom("a$i")
                        functors[t][i] = symbols.functor(atoms[t][i], i % 5)
                    }
                }
            }.also { start.countDown() }
            .forEach { it.join() }
        for (i in 0 until names) {
            val atom = atoms[0][i]
            assertEquals("a$i", symbols.atoms[atom])
            assertEquals(atom, symbols.functorNames[functors[0][i]])
            assertEquals(i % 5, symbols.functorArities[functors[0][i]])
            for (t in 1 until threads) {
                assertEquals(atom, atoms[t][i])
                assertEquals(functors[0][i], functors[t][i])
            }
        }
        assertEquals(names + 1, symbols.atoms.size - Symbols.of(null).atoms.size)
        assertEquals(symbols.atom("before"), Symbols.shared(symbols).atom("before"))
    }
}
