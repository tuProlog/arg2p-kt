package it.unibo.tuprolog.argumentation.parallel

import it.unibo.tuprolog.argumentation.Flags
import it.unibo.tuprolog.argumentation.Semantics
import it.unibo.tuprolog.argumentation.parallel.ThreadedSolver
import kotlin.test.Test
import kotlin.test.assertEquals

// Query mode: the graph built on the arguments the evaluations depend on, as the semantics declares
class QueryModeTest {
    // two parts no attack relates: a and b, and c, d and -d
    private val theory = "r1 :=> a.\nr2 : a => b.\nr3 :=> c.\nr4 : c => d.\nr5 :=> -d.\n"

    private fun parallel(flags: Flags) = ThreadedSolver(threads = 2, parts = 2, flags = flags).also { it.load(theory) }

    @Test
    fun theGraphIsBuiltOnTheRulesTheQueryDependsOn() {
        val parallel = parallel(Flags.defaults().graphExtensions())
        assertEquals(1, parallel.solve("b").inArgs.size)
        assertEquals(2, parallel.evaluation!!.arguments)
        // a later query builds its own graph, as the standard solver does
        assertEquals(1, parallel.solve("d").undArgs.size)
        assertEquals(3, parallel.evaluation!!.arguments)
        assertEquals(1, parallel.solve("a").inArgs.size)
        assertEquals(1, parallel.evaluation!!.arguments)
        // the same query again is answered on the graph built for it
        assertEquals(1, parallel.solve("a").inArgs.size)
        assertEquals(0, parallel.evaluation!!.added)
    }

    @Test
    fun theAttackersOfTheArgumentsAreBuiltAsWell() {
        val parallel = parallel(Flags.defaults().graphExtensions())
        assertEquals(1, parallel.solve("d").undArgs.size)
        assertEquals(3, parallel.evaluation!!.arguments)
    }

    @Test
    fun theWholeGraphIsBuiltWithNoQueryMode() {
        val parallel = parallel(Flags.defaults().graphExtensions().queryMode(false))
        assertEquals(1, parallel.solve("b").inArgs.size)
        assertEquals(5, parallel.evaluation!!.arguments)
    }

    @Test
    fun theWholeGraphIsBuiltForASemanticsDependingOnIt() {
        val parallel = parallel(Flags.defaults().graphExtensions().argumentLabellingMode(Semantics.STABLE))
        parallel.solve("b")
        assertEquals(5, parallel.evaluation!!.arguments)
    }

    @Test
    fun aConnectedSemanticsBuildsWhatTheQueryIsConnectedTo() {
        // naive: the arguments for a and for b, which a supports
        val parallel = parallel(Flags.defaults().graphExtensions().argumentLabellingMode(Semantics.NAIVE))
        parallel.solve("a")
        assertEquals(2, parallel.evaluation!!.arguments)
    }

    @Test
    fun aChangeOfTheTheoryBuildsTheGraphAnew() {
        val parallel = parallel(Flags.defaults().graphExtensions())
        assertEquals(1, parallel.solve("b").inArgs.size)
        parallel.add("r6 :=> -a")
        assertEquals(1, parallel.solve("b").undArgs.size)
        assertEquals(3, parallel.evaluation!!.arguments)
    }
}
