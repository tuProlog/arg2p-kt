package it.unibo.tuprolog.argumentation.core.libraries.framework.utils.helpers

import it.unibo.tuprolog.argumentation.core.TestingUtils.solverWithTheory
import it.unibo.tuprolog.core.Atom
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.TermComparator
import it.unibo.tuprolog.core.parsing.parse
import kotlin.random.Random
import kotlin.test.Test
import kotlin.test.assertEquals

/** Sorting many atoms new to the machine at once (ranked in a batch) gives the standard order of tuProlog */
class AtomOrderTest {
    // [count] atom names, some equal, in a random order
    private fun names(
        count: Int,
        seed: Int,
    ): List<String> {
        val random = Random(seed)
        return List(count) { i -> "a" + random.nextInt(count / 2) + (if (i % 7 == 0) "_x" else "") }
    }

    private fun sorted(
        goal: String,
        variable: String,
    ): List<String> {
        val solver = solverWithTheory("")
        val solution = solver.solve(Struct.parse(goal, solver.operators)).first()
        val bound =
            solution.substitution.entries
                .firstOrNull { it.key.name == variable }
                ?.value
                ?: error("no $variable: ${if (solution.isHalt) solution.exception?.message else solution.toString().take(300)}")
        return bound
            .castToList()
            .toList()
            .map { it.toString() }
    }

    // (utils::sortDistinct: the reverse of the standard order, each term once)
    private fun expected(terms: List<Term>): List<String> =
        terms
            .sortedWith(TermComparator.DefaultComparator)
            .map { it.toString() }
            .distinct()
            .reversed()

    @Test
    fun manyNewAtomsSortAsTuProlog() {
        val atoms = names(500, 1).map { Atom.of(it) }
        val list = atoms.joinToString(", ", "[", "]")
        assertEquals(expected(atoms), sorted("utils::sortDistinct($list, S)", "S"))
    }

    @Test
    fun newAtomsAmongRankedOnesSortAsTuProlog() {
        // a few ranked one by one first, then many new ones mixed with them, in terms as well
        val first = names(20, 2).map { Atom.of(it) }
        val second = names(400, 3).map { Atom.of(it) } + first
        val terms = second.mapIndexed { i, atom -> if (i % 3 == 0) Struct.of("f", atom, Atom.of("k$i")) else atom }
        val goal =
            "utils::sortDistinct(${first.joinToString(", ", "[", "]")}, _), " +
                "utils::sortDistinct(${terms.joinToString(", ", "[", "]")}, S)"
        assertEquals(expected(terms), sorted(goal, "S"))
    }
}
