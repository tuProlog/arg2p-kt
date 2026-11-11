package it.unibo.tuprolog.argumentation.federation

import it.unibo.tuprolog.argumentation.parallel.ThreadedSolver
import kotlin.js.Promise
import kotlin.test.Test
import kotlin.test.assertEquals

// Parallel solvers of Node.js federated: the one evaluating places consumers on the threads of the other
class FederatedNodeSolverTest {
    // a chain whose last conclusion is rebutted by an argument resting on another chain, and a chain apart
    private val theory =
        (1..6).joinToString("\n") { "r$it : a${it - 1} => a$it." } +
            "\nf0 :=> a0.\n" +
            (1..6).joinToString("\n") { "s$it : b${it - 1} => b$it." } +
            "\ng0 :=> b0.\nt : b6 => -a6.\nsup(t, r6).\nu0 :=> c0.\nu1 : c0 => c1.\n"

    private fun claims(
        parallel: ThreadedSolver,
        statement: String,
    ): Promise<List<List<String>>> =
        parallel.solve(statement).then { response ->
            listOf(response.inArgs, response.outArgs, response.undArgs).map { it.map { r -> r.claim } }
        }

    // (the promise a step gives, the next step on its value: as JavaScript flattens promises)
    private fun <T, R> Promise<T>.step(next: (T) -> Promise<R>): Promise<R> = then { next(it) }.unsafeCast<Promise<R>>()

    // The consumers of the graph of "a6" on both parallel solvers
    private fun builtByBoth(
        evaluating: ThreadedSolver,
        lending: ThreadedSolver,
    ): Promise<Unit> =
        claims(evaluating, "a6")
            .step {
                assertEquals(listOf(emptyList(), listOf("a6"), emptyList()), it)
                val sites =
                    evaluating.evaluation!!
                        .workers
                        .map { it.site }
                        .toSet()
                assertEquals(setOf(evaluating.id, lending.id), sites, "the consumers on both solvers")
                evaluating.close()
            }.step { lending.close() }

    @Test
    fun twoSolversOverWebSockets(): Promise<Unit> {
        // (each a node, a solver and one consumer: the third part on the lending one)
        val lending = ThreadedSolver(threads = 2)
        val evaluating = ThreadedSolver(threads = 2, parts = 3)
        evaluating.load(theory)
        return Federation(lending)
            .listen(0)
            .step { port -> Federation(evaluating).connect("ws://127.0.0.1:$port") }
            .step { builtByBoth(evaluating, lending) }
    }

    // A wrong code rejected, the right one admitted
    @Test
    fun aSolverRequiringACode(): Promise<Unit> {
        val lending = ThreadedSolver(threads = 2)
        val evaluating = ThreadedSolver(threads = 2, parts = 3)
        evaluating.load(theory)
        val listening = Federation(lending).also { it.requiresCode = true }
        val connecting = Federation(evaluating)
        return listening
            .listen(0)
            .step { port ->
                val url = "ws://127.0.0.1:$port"
                val wrong = if (listening.code == "0000") "0001" else "0000"
                connecting
                    .connect(url, wrong)
                    .then({ throw IllegalStateException("A wrong code admitted") }, { })
                    .step { connecting.connect(url, listening.code) }
            }.step { builtByBoth(evaluating, lending) }
    }

    // Over a medium of the page: what a new medium has to provide is a link
    @Test
    fun twoSolversOverAMediumOfThePage(): Promise<Unit> {
        val lending = ThreadedSolver(threads = 2)
        val evaluating = ThreadedSolver(threads = 2, parts = 3)
        evaluating.load(theory)
        val (one, other) = listOf(evaluating, lending).map { Federation(it) }
        lateinit var ends: Pair<ExternalLink, ExternalLink>
        val first = ExternalLink({ frame -> other.linkReceived(ends.second, frame) }, { other.linkClosed(ends.second) }, outgoing = true)
        val second = ExternalLink({ frame -> one.linkReceived(ends.first, frame) }, { one.linkClosed(ends.first) })
        ends = first to second
        one.linkOpened(first)
        other.linkOpened(second)
        return Promise<Unit> { resolve, _ -> one.onPeersChanged = { if (one.peers.isNotEmpty()) resolve(Unit) } }
            .step { builtByBoth(evaluating, lending) }
    }
}
