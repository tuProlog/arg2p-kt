package it.unibo.tuprolog.argumentation.core.runtime.context

import it.unibo.tuprolog.argumentation.core.TestingUtils.solverWithTheory
import it.unibo.tuprolog.core.Atom
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.solve.Solver
import kotlin.test.Test
import kotlin.test.assertEquals

// The graph saved a batch at a time is the one the predicates of the module graphStore save, as they read it
class GraphStoreBatchTest {
    private val a = "[[f1], none, [a], [], [[], [], [f1]]]"
    private val b = "[[r1, f1], r1, [b], [a], [[r1], [r1], [f1]]]"
    private val c = "[[f2], none, ['-'(b)], [], [[], [], [f2]]]"

    private val reads =
        listOf(
            "graphStore::argumentById(Id, A)",
            "graphStore::argumentFor(C, A)",
            "graphStore::argumentInfo(Id, I, Info)",
            "graphStore::newConclusion(a, C)",
            "graphStore::support(S, A)",
            "graphStore::supportedById(I, J)",
            "graphStore::attack(T, B, A, O)",
            "graphStore::attackById(T, IB, IA, IO)",
            "graphStore::attacksById(IB, IA)",
        )

    private fun read(solver: Solver) =
        reads.map { goal ->
            solver
                .solve(Struct.parse(goal, solver.operators))
                .filter { it.isYes }
                .map { solution -> solution.substitution.entries.joinToString { "${it.key.name}=${it.value}" } }
                .toList()
                .sorted()
        }

    @Test
    fun theBatchSavesWhatThePredicatesSave() {
        val predicates = solverWithTheory("")
        predicates
            .solve(
                Struct.parse(
                    "context_reset, " +
                        "graphStore::saveArgument(a, [a], $a), graphStore::saveArgument(a, [b], $b), " +
                        "graphStore::saveArgument(a, ['-'(b)], $c), graphStore::saveSupport($a, $b), " +
                        "graphStore::saveAttack(rebut, $c, $b, $b), graphStore::saveAttack(undermine, $b, $c, $c)",
                    predicates.operators,
                ),
            ).first()

        val batch = solverWithTheory("")
        batch.solve(Struct.parse("context_reset", batch.operators)).first()
        val context = Context.of(batch.libraries)!!
        val (ta, tb, tc) = listOf(a, b, c).map { Struct.parse(it, batch.operators) }
        GraphStoreBatch.saveArguments(context, Atom.of("a"), listOf(ta, tb, tc))
        GraphStoreBatch.saveSupports(context, listOf(ta to tb))
        GraphStoreBatch.saveAttacks(
            context,
            listOf(
                GraphStoreBatch.Attack(Atom.of("rebut"), tc, tb, tb),
                GraphStoreBatch.Attack(Atom.of("undermine"), tb, tc, tc),
            ),
        )

        assertEquals(read(predicates), read(batch))
    }
}
