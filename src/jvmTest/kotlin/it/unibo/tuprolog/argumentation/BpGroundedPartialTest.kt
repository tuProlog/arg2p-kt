package it.unibo.tuprolog.argumentation

import kotlin.random.Random
import kotlin.test.Test
import kotlin.test.assertEquals

// bp_grounded_partial on random theories (rules, conflicts, preferences, burdens): one labelling, each argument labelled once
// (its demonstrations commit through the cut of argumentLabelling, and go through every complement of a conclusion)
class BpGroundedPartialTest {
    private fun theory(random: Random): String {
        val atoms = listOf("a", "b", "c", "d", "e")
        val lines = ArrayList<String>()
        repeat(random.nextInt(2, 5)) { lines += "f$it :=> ${atoms[random.nextInt(atoms.size)]}." }
        repeat(random.nextInt(3, 8)) { i ->
            val premises = List(random.nextInt(0, 3)) { atoms[random.nextInt(atoms.size)] }.distinct()
            val head = (if (random.nextBoolean()) "-" else "") + atoms[random.nextInt(atoms.size)]
            lines += "r$i : ${if (premises.isEmpty()) "[]" else premises.joinToString(", ")} => $head."
        }
        repeat(random.nextInt(0, 3)) { lines += "sup(r${random.nextInt(3)}, r${random.nextInt(3, 7)})." }
        repeat(random.nextInt(1, 3)) { lines += "bp(${if (random.nextBoolean()) "-" else ""}${atoms[random.nextInt(atoms.size)]})." }
        return lines.joinToString("\n") + "\n"
    }

    private val flags =
        "statementLabellingMode(statement).\nargumentLabellingMode(bp_grounded_partial).\norderingPrinciple(last).\n" +
            "orderingComparator(elitist).\ngraphExtension(standardPref).\ngraphExtension(bp).\nqueryMode(off).\n"

    @Test
    fun eachArgumentIsLabelledOnce() {
        val random = Random(20261110)
        val problems = ArrayList<String>()
        repeat(300) {
            val text = theory(random)
            val arg2p = Arg2p.of(text + flags)
            val solutions =
                arg2p
                    .solve(
                        "arg2p::solve, store::arguments(As), length(As, N), findall(A-L, store::argumentLabel(L, A), Ls), length(Ls, M)",
                    ).toList()
            val yes = solutions.filter { it.isYes }
            val halts = solutions.filter { it.isHalt }
            when {
                halts.isNotEmpty() -> problems += "HALT ${halts.first().error}\n$text"
                yes.size != 1 -> problems += "${yes.size} solutions\n$text"
                yes.first().text("N") != yes.first().text("M") ->
                    problems +=
                        "args ${yes.first().text("N")} labels ${yes.first().text("M")}\n$text"
            }
        }
        assertEquals(emptyList(), problems.take(3))
    }
}
