package it.unibo.tuprolog.argumentation.core.libraries.framework.store.helpers.sharing

import it.unibo.tuprolog.argumentation.core.Arg2pSolverFactory
import it.unibo.tuprolog.argumentation.core.libraries.ArgLibrary
import it.unibo.tuprolog.argumentation.core.libraries.ArgsFlag
import it.unibo.tuprolog.argumentation.engine.natives.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.engine.natives.NativeCall
import it.unibo.tuprolog.argumentation.engine.vm.program.SolverSession
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.Solver
import it.unibo.tuprolog.solve.library.Library
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertTrue

/** The graph a node of a session built, merged into the store of another as it is: the other builds on it */
class GraphSegmentsTest {
    // segmentOut/0 takes the graph of the solver since its last segment, segmentIn/0 merges the one taken last
    private class Segments : ArgLibrary {
        var segment: GraphSegment? = null
        var mark = GraphMark.START
        var merged = IntArray(0)

        override val alias = "test.segments"

        private val out =
            object : AbstractNativePrimitive(Signature("segmentOut", 0)) {
                override fun call(call: NativeCall): Int {
                    val (taken, next) = call.graphSegmentSince(mark)
                    segment = taken
                    mark = next
                    return NativeCall.CONTINUE
                }
            }

        private val into =
            object : AbstractNativePrimitive(Signature("segmentIn", 0)) {
                override fun call(call: NativeCall): Int {
                    merged = call.mergeGraphSegment(shared!!.segment!!, "f")
                    return NativeCall.CONTINUE
                }
            }

        var shared: Segments? = null

        override val baseContent: Library
            get() = Library.of(alias, primitives = listOf(out, into).associateBy { it.signature })

        override val baseFlags: Iterable<ArgsFlag>
            get() = emptyList()
    }

    private fun Solver.holds(goal: String) = solveOnce(Struct.parse(goal, operators)).isYes

    private fun Solver.count(goal: String): Int =
        solveOnce(Struct.parse("findall(x, ($goal), Xs), length(Xs, N)", operators))
            .substitution.values
            .last()
            .toString()
            .toInt()

    @Test
    fun aNodeBuildsOnTheGraphOfAnother() {
        val session = SolverSession()
        val first = Segments()
        val second = Segments().also { it.shared = first }
        val a = Arg2pSolverFactory.default("f1 :=> a. r1 : a => b.", staticLibs = listOf(first)).also { SolverSession.join(it, session) }
        val b = Arg2pSolverFactory.default("r2 : b => c. f2 :=> -b.", staticLibs = listOf(second)).also { SolverSession.join(it, session) }
        // a builds its graph whole, b loads its rules
        assertTrue(a.holds("context_reset, pipeline::graph(whole, whole_graph)"))
        assertTrue(a.holds("segmentOut"))
        assertEquals(2, first.segment!!.size)
        assertTrue(b.holds("context_reset, segmentIn"))
        assertEquals(2, second.merged.size)
        // the arguments of a, as b reads them, and an argument of b's rules on b's conclusion
        assertTrue(b.holds("store::argument(A), store::conclusion(A, [b])"))
        assertTrue(
            b.holds(
                "findall(C, store::newConclusion(f, [C]), Cs), argumentBuilder::buildArgumentsFromRules(Cs, a, base), " +
                    "store::argument(A), store::conclusion(A, [c])",
            ),
        )
        // merged again: nothing new
        assertTrue(b.holds("segmentIn"))
        assertEquals(0, second.merged.size)
        assertEquals(1, b.count("store::argument(A), store::conclusion(A, [b])"))
        // the same identifiers on both nodes
        assertEquals(2, a.count("context_check(arg(_))"))
    }
}
