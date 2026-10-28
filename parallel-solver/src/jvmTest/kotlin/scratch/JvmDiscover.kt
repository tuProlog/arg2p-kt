package scratch

import it.unibo.tuprolog.argumentation.parallel.cluster.LocalCluster
import it.unibo.tuprolog.argumentation.parallel.cluster.ThreadedCluster
import kotlin.test.Test

// A JVM cluster discovering the Node.js ones on beacon port 29431 (ARG2P_DISCOVER set), then evaluating with them
class JvmDiscover {
    @Test
    fun discover() {
        if (System.getenv("ARG2P_DISCOVER") == null) return
        val theory =
            "f0 :=> c0.\ng0 :=> d0.\n" + (1..10).joinToString("\n") { "r$it : c${it - 1} => c$it.\ns$it : d${it - 1} => d$it." } +
                "\nt : d10 => -c10.\nsup(t, r10).\n"
        ThreadedCluster(threads = 1).use { cluster ->
            cluster.discover(29431)
            val deadline = System.currentTimeMillis() + 60_000
            while (cluster.site.peerSites.size < 2 && System.currentTimeMillis() < deadline) Thread.sleep(50)
            println("DISCOVER JVM peers ${cluster.site.peerSites.size}")
            cluster.load(theory)
            val local = LocalCluster().also { it.load(theory) }
            val labels = { r: it.unibo.tuprolog.argumentation.parallel.message.EvaluationResponse ->
                listOf(r.inArgs, r.outArgs, r.undArgs).map { l ->
                    l.map { it.claim }
                }
            }
            listOf("c10", "-c10", "d10").forEach { s ->
                val same = labels(local.solve(s)) == labels(cluster.solve(s))
                println("DISCOVER JVM $s ${if (same) "same" else "DIFFERENT"}")
            }
            println("DISCOVER JVM workers run here ${cluster.site.workersRun}")
            Thread.sleep(25_000)
        }
    }
}
