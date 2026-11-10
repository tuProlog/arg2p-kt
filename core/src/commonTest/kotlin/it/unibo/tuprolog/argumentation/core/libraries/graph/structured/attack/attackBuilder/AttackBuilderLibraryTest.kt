package it.unibo.tuprolog.argumentation.core.libraries.graph.structured.attack.attackBuilder

import it.unibo.tuprolog.argumentation.core.LibraryTesting
import kotlin.test.Test
import kotlin.test.assertEquals

class AttackBuilderLibraryTest {
    // -a undermines the premise a, and through it the argument for b
    private val module =
        LibraryTesting("f1 :=> a.\nr1 : a => b.\nf2 :=> -a.").also {
            it.run("context_reset", "store::rulesRead", "argumentBuilder::buildArguments", "attackBuilder::buildAttacks")
        }

    private fun attacked(attacker: String) =
        module.values("store::attack(_, A, B, _), store::claim(A, $attacker), store::claim(B, C)", "C").toSet()

    @Test
    fun directAndTransitiveAttacks() {
        assertEquals(setOf("a", "b"), attacked("'-'(a)"))
        assertEquals(setOf("'-'(a)"), attacked("a"))
    }

    // An argument undercutting its own rule attacks itself: under grounded it is undecided
    @Test
    fun anArgumentAttackingItself() {
        val module =
            LibraryTesting(
                "f1 :=> a.\nr1 : a => undercut(r1).\nstatementLabellingMode(statement).\n" +
                    "argumentLabellingMode(grounded).",
            )
        assertEquals("[[undercut(r1)]]", module.value("solve(_, _, U)", "U"))
    }
}
