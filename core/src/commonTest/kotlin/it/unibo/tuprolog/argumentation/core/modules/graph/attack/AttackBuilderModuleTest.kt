package it.unibo.tuprolog.argumentation.core.modules.graph.attack

import it.unibo.tuprolog.argumentation.core.ModuleTesting
import kotlin.test.Test
import kotlin.test.assertEquals

class AttackBuilderModuleTest {
    // -a undermines the premise a, and through it the argument for b
    private val module =
        ModuleTesting("f1 :=> a.\nr1 : a => b.\nf2 :=> -a.").also {
            it.run("context_reset", "parser:::convertAllRules(_)", "argumentBuilder::buildArguments", "attackBuilder::buildAttacks")
        }

    private fun attacked(attacker: String) =
        module.values("graphStore::attack(_, A, B, _), argument::claim(A, $attacker), argument::claim(B, C)", "C").toSet()

    @Test
    fun directAndTransitiveAttacks() {
        assertEquals(setOf("a", "b"), attacked("'-'(a)"))
        assertEquals(setOf("'-'(a)"), attacked("a"))
    }

    // An argument undercutting its own rule attacks itself: under grounded it is undecided
    @Test
    fun anArgumentAttackingItself() {
        val module =
            ModuleTesting(
                "f1 :=> a.\nr1 : a => undercut(r1).\nstatementLabellingMode(statement).\n" +
                    "argumentLabellingMode(grounded).",
            )
        assertEquals("[[undercut(r1)]]", module.value("buildLabelSets(_, _, U)", "U"))
    }
}
