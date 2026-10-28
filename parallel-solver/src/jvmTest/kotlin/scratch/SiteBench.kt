package scratch

import it.unibo.tuprolog.argumentation.Flags
import it.unibo.tuprolog.argumentation.parallel.cluster.ThreadedCluster
import it.unibo.tuprolog.argumentation.parallel.federation.MemoryLinks
import java.util.concurrent.Executors
import kotlin.test.Test

class SiteBench {
    private fun theory(
        groups: Int,
        length: Int,
    ) = (0 until groups).joinToString("\n") { g ->
        (0 until length).joinToString("\n") { i ->
            if (i ==
                0
            ) {
                "f_${g}_0 :=> a_${g}_0."
            } else {
                "r_${g}_$i : a_${g}_${i - 1} => a_${g}_$i.\nu_${g}_$i : a_${g}_${i - 1} => -a_${g}_$i."
            }
        }
    }

    private fun time(
        clusters: List<ThreadedCluster>,
        text: String,
        queries: List<String>,
    ): String {
        val t0 = System.nanoTime()
        clusters[0].load(text)
        val pool = Executors.newFixedThreadPool(queries.size)
        val und = queries.map { q -> pool.submit<Int> { clusters[0].solve(q).undArgs.size } }.sumOf { it.get() }
        pool.shutdown()
        clusters.forEach { it.close() }
        return "${(System.nanoTime() - t0) / 1_000_000}ms (und $und)"
    }

    @Test
    fun probe() {
        if (System.getenv("ARG2P_PROBE") == null) return
        val flags = Flags.defaults().graphExtensions()
        for ((name, groups, length) in listOf(Triple("32x30", 32, 30), Triple("4x120", 4, 120))) {
            val text = theory(groups, length)
            val queries = (0 until groups).map { "a_${it}_${length - 1}" }
            repeat(3) { round ->
                val one = time(listOf(ThreadedCluster(threads = 22, flags = flags)), text, queries)
                val a = ThreadedCluster(threads = 11, flags = flags)
                val b = ThreadedCluster(threads = 11)
                MemoryLinks.connect(a.site, b.site)
                val two = time(listOf(a, b), text, queries)
                println("SITEBENCH $name round $round one site $one, two sites $two")
            }
        }
    }
}
