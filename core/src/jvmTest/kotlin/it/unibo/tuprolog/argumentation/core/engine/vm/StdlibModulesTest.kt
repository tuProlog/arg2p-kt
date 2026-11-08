package it.unibo.tuprolog.argumentation.core.engine.vm

import it.unibo.tuprolog.argumentation.Flags
import it.unibo.tuprolog.argumentation.core.Arg2pSolverFactory
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.TermComparator
import it.unibo.tuprolog.core.parsing.parse
import kotlin.test.Test
import kotlin.test.assertEquals
import it.unibo.tuprolog.core.List as LogicList

/** The natives of the standard library within the solvers of arg2p */
class StdlibModulesTest {
    // A theory defining a predicate keeps it, native or not
    @Test
    fun aTheoryKeepsItsPredicates() {
        val solver = Arg2pSolverFactory.default(theory = "functor(a, mine, 7).\nlength(a, mine).\n", settings = Flags.defaults().library())
        val goal = Struct.parse("functor(a, N, A), length(a, L)", solver.operators)
        assertEquals(
            "[N, A, L] = [mine, 7, mine]",
            solver.solve(goal).first().substitution.let { s ->
                "[N, A, L] = [${s.getByName("N")}, ${s.getByName("A")}, ${s.getByName("L")}]"
            },
        )
    }

    // The sorts of the module utils, on words, in the order of tuProlog
    @Test
    fun theSortsFollowTheOrderOfTuProlog() {
        val ground =
            listOf(
                "1",
                "-3",
                "0",
                "2.5",
                "1.0",
                "100000000000",
                "a",
                "b",
                "'B'",
                "'é'",
                "'_x'",
                "'a b'",
                "[]",
                "{}",
                "[a]",
                "[b]",
                "[a, b]",
                "[a, c]",
                "f(a)",
                "f(b)",
                "g(a)",
                "f(a, b)",
                "f(b, a)",
                "g(f(a), [1, 2])",
                "a",
                "f(a)",
                "[a]",
                "1",
                "'.'(a, b)",
                "(a, b)",
                "- a",
            )
        val solver = Arg2pSolverFactory.default(theory = "", settings = Flags.defaults().library())
        val items = ground.map { Term.parse(it, solver.operators) }
        val sorted = items.sortedWith(TermComparator.DefaultComparator)
        val distinct =
            sorted.fold(mutableListOf<Term>()) { acc, t ->
                acc.also {
                    if (it.isEmpty() ||
                        TermComparator.DefaultComparator.compare(it.last(), t) != 0
                    ) {
                        it.add(t)
                    }
                }
            }
        val text = "[" + ground.joinToString(", ") + "]"

        fun result(name: String): String =
            solver
                .solve(Struct.parse("utils::$name($text, R)", solver.operators))
                .first()
                .substitution
                .getByName("R")
                .toString()
        assertEquals(LogicList.of(sorted.asReversed()).toString(), result("sortDescending"))
        assertEquals(LogicList.of(distinct).toString(), result("deduplicate"))
        assertEquals(LogicList.of(distinct.asReversed()).toString(), result("sortDistinct"))
    }
}
