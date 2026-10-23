package it.unibo.tuprolog.argumentation.core.modules.query

import it.unibo.tuprolog.argumentation.core.ModuleTesting
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertTrue

// The graph explored one argument at a time (buildArgument/2, findAttacker/4), as the actor solver does
class StructuredModuleTest {
    // c from the premise b, which r5 undercuts, and -c rebutting c
    private val theory = "r1 :=> b.\nr2 : b => c.\nr5 :=> undercut(r1).\nr6 :=> -c."

    private fun module() =
        ModuleTesting(theory).also {
            it.run("context_reset", "parser:::convertAllRules(_)")
        }

    @Test
    fun theArgumentsForAQuery() {
        val module = module()
        assertEquals(listOf("[[r2, r1], r2, [c], [b], [[r2], [r2], [r1]]]"), module.values("structured::buildArgument(c, A)", "A"))
        assertTrue(module.holds("structured::buildArgument(c, A), context_check(argument(A))"))
    }

    @Test
    fun theAttackersOfAnArgument() {
        val module = module()
        val target = module.value("structured::buildArgument(c, A)", "A")!!
        val attackers = module.values("structured::findAttacker($target, [], B, yes), argument::claim(B, C)", "C").toSet()
        // the undercut of the premise b, and the rebuttal of c
        assertEquals(setOf("undercut(r1)", "'-'(c)"), attackers)
        assertTrue(module.holds("graphStore::attack(undercut, B, $target, _), argument::claim(B, undercut(r1))"))
    }

    @Test
    fun anAttackerInTheQueryChainIsACycle() {
        val module = module()
        val target = module.value("structured::buildArgument(c, A)", "A")!!
        val undercut = module.value("structured::buildArgument(undercut(r1), A)", "A")!!
        assertTrue(module.holds("structured::findAttacker($target, [$undercut], $undercut, no)"))
    }
}
