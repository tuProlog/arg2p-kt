package scratch

import it.unibo.tuprolog.argumentation.Flags
import it.unibo.tuprolog.argumentation.core.Arg2pSolverFactory
import it.unibo.tuprolog.argumentation.parallel.cluster.ThreadedCluster
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import kotlin.test.Test

// A JVM site for the Node.js one to join (ARG2P_HUB set): it evaluates its theory with the workers of both
class JvmHub {
    @Test
    fun hub() {
        if (System.getenv("ARG2P_HUB") == null) return
        val n = 10
        val theory =
            "f0 :=> c0.\ng0 :=> d0.\n" + (1..n).joinToString("\n") { "r$it : c${it - 1} => c$it.\ns$it : d${it - 1} => d$it." } +
                "\nt : d$n => -c$n.\nsup(t, r$n).\n"
        ThreadedCluster(threads = 1).use { cluster ->
            println("HUB listening on ${cluster.listen(9431, "127.0.0.1")}")
            val deadline = System.currentTimeMillis() + 60_000
            while (cluster.site.peerSites.isEmpty() && System.currentTimeMillis() < deadline) Thread.sleep(50)
            println("HUB peers ${cluster.site.peerSites}")
            cluster.load(theory)
            val solver = Arg2pSolverFactory.default(theory = theory, settings = Flags.defaults().library())
            solver.solve(Struct.parse("buildLabelSetsSilent", solver.operators)).first()
            listOf("c$n", "-c$n", "d$n", "c3").forEach { statement ->
                val labels = { label: String ->
                    solver
                        .solve(
                            Struct.parse("graphStore::argumentLabel($label, A), argument::claim(A, $statement)", solver.operators),
                        ).count { it.isYes }
                }
                val r = cluster.solve(statement)
                val same =
                    listOf(r.inArgs.size, r.outArgs.size, r.undArgs.count { it.queryChain.isNotEmpty() }) ==
                        listOf(labels("in"), labels("out"), labels("und"))
                println(
                    "HUB $statement ${if (same) "same" else "DIFFERENT"} in ${r.inArgs.map { it.claim }} out ${r.outArgs.map { it.claim }}",
                )
            }
            println("HUB workers run here: ${cluster.site.workersRun} of 3")
            // the Node.js site evaluates its own theory meanwhile, with the workers of this one
            Thread.sleep(20_000)
            println("HUB done")
        }
    }
}
