package scratch

import it.unibo.tuprolog.argumentation.Flags
import it.unibo.tuprolog.argumentation.parallel.cluster.ThreadedCluster
import it.unibo.tuprolog.argumentation.parallel.federation.MemoryLinks
import java.util.concurrent.CompletableFuture
import java.util.concurrent.TimeUnit
import kotlin.test.Test

class TranspositionProbe {
    @Test
    fun probe() {
        if (System.getenv("ARG2P_PROBE") == null) return
        val theory = "autoTransposition.\ns1 : a -> b.\nf1 :=> a.\nf2 :=> -b."
        val (evaluating, first, second) =
            listOf(
                ThreadedCluster(threads = 1, flags = Flags.defaults().graphExtensions()),
                ThreadedCluster(threads = 2),
                ThreadedCluster(threads = 2),
            )
        MemoryLinks.connect(evaluating.site, first.site)
        MemoryLinks.connect(evaluating.site, second.site)
        MemoryLinks.connect(first.site, second.site)
        evaluating.load(theory)
        listOf("a", "b", "-a", "-b").forEach { s ->
            val f = CompletableFuture<Any>()
            evaluating.site.solve(s) { f.complete(it) }
            println("DISTRIBUTED $s ${runCatching { f.get(10, TimeUnit.SECONDS) }}")
            if (!f.isDone) {
                Thread.getAllStackTraces().filter { it.key.name.startsWith("arg2p") }.forEach { (t, st) ->
                    println("THREAD ${t.name} ${t.state}")
                    st.filter { it.className.startsWith("it.unibo") }.take(25).forEach { println("THREAD   at $it") }
                }
                return
            }
        }
    }
}
