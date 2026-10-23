package it.unibo.tuprolog.argumentation.core.modules.graph.argument

import it.unibo.tuprolog.argumentation.core.ModuleTesting
import kotlin.test.Test
import kotlin.test.assertEquals

class ArgumentBuilderModuleTest {
    // Two chains of rules on a premise, one closing a cycle that makes no argument
    private val module =
        ModuleTesting("f1 :=> a.\nr1 : a => b.\nr2 : b => c.\nr3 : c, d => e.\nr4 : e => d.").also {
            it.run("context_reset", "parser:::convertAllRules(_)", "argumentBuilder::buildArguments")
        }

    @Test
    fun theArgumentsOfTheRulesSupported() {
        val claims = module.values("graphStore::argument(A), argument::claim(A, C)", "C").toSet()
        assertEquals(setOf("a", "b", "c"), claims)
    }

    @Test
    fun theSupportsOfTheArguments() {
        val supported = module.values("graphStore::support(S, A), argument::claim(S, a), argument::claim(A, C)", "C")
        assertEquals(listOf("b"), supported)
    }
}
