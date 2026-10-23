package it.unibo.tuprolog.argumentation.core.modules.query

import it.unibo.tuprolog.argumentation.core.ModuleTesting
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertTrue

// The graph explored one argument at a time (buildArgument/3, findAttacker/6), as the actor solver does
class StructuredModuleTest {
    // c from the premise b, which -b undermines; r3 undercuts the rule of c, r5 the premise (not an attack)
    private val theory = "r1 :=> b.\nr2 : b => c.\nr3 :=> undercut(r2).\nr4 :=> -b.\nr5 :=> undercut(r1)."

    private fun module(theory: String = this.theory) =
        ModuleTesting(theory).also {
            it.run("context_reset", "parser:::convertAllRules(_)")
        }

    @Test
    fun theArgumentsForAQuery() {
        val module = module()
        assertEquals(listOf("[[r2, r1], r2, [c], [b], [[r2], [r2], [r1]]]"), module.values("structured::buildArgument(c, A, _)", "A"))
        assertTrue(module.holds("structured::buildArgument(c, A, _), context_check(argument(A))"))
        assertEquals(listOf("b"), module.values("structured::buildArgument(c, _, S), member(A, S), argument::claim(A, C)", "C"))
    }

    @Test
    fun theAttackersOfAnArgumentAndOfItsSubArguments() {
        val module = module()
        val target = module.value("structured::buildArgument(c, A, _)", "A")!!
        val subArguments = module.value("structured::buildArgument(c, _, S)", "S")!!
        val attackers = module.values("structured::findAttacker($target, $subArguments, [], B, _, yes), argument::claim(B, C)", "C")
        // the undercut of the rule of c, and the undermining of b: a premise is not undercut
        assertEquals(setOf("undercut(r2)", "'-'(b)"), attackers.toSet())
        assertEquals(2, attackers.size)
        assertTrue(module.holds("graphStore::attack(undermine, B, $target, _), argument::claim(B, '-'(b))"))
    }

    @Test
    fun theAttackersOfAnArgumentEvaluatedAgain() {
        val module = module()
        val target = module.value("structured::buildArgument(c, A, _)", "A")!!
        val subArguments = module.value("structured::buildArgument(c, _, S)", "S")!!
        val query = "structured::findAttacker($target, $subArguments, [], B, _, yes), argument::claim(B, C)"
        assertEquals(module.values(query, "C"), module.values(query, "C"))
    }

    @Test
    fun theAttackersOfAnArgumentBuiltElsewhere() {
        val builder = module()
        val target = builder.value("structured::buildArgument(c, A, _)", "A")!!
        val subArguments = builder.value("structured::buildArgument(c, _, S)", "S")!!
        // a solver without the rules of the argument
        val module = module("r4 :=> -b.\nr5 :=> undercut(r1).")
        val attackers = module.values("structured::findAttacker($target, $subArguments, [], B, _, yes), argument::claim(B, C)", "C")
        assertEquals(listOf("'-'(b)"), attackers)
    }

    @Test
    fun anAttackerInTheQueryChainIsACycle() {
        val module = module()
        val target = module.value("structured::buildArgument(c, A, _)", "A")!!
        val subArguments = module.value("structured::buildArgument(c, _, S)", "S")!!
        val undercut = module.value("structured::buildArgument(undercut(r2), A, _)", "A")!!
        assertTrue(module.holds("structured::findAttacker($target, $subArguments, [$undercut], $undercut, [], no)"))
    }
}
