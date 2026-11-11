package it.unibo.tuprolog.argumentation.core.libraries.graph.structured.argument.argumentBuilder

import it.unibo.tuprolog.argumentation.core.LibraryTesting
import kotlin.test.Test
import kotlin.test.assertEquals

class ArgumentBuilderLibraryTest {
    // Two chains of rules on a premise, one closing a cycle that makes no argument
    private val module =
        LibraryTesting("f1 :=> a.\nr1 : a => b.\nr2 : b => c.\nr3 : c, d => e.\nr4 : e => d.").also {
            it.run("contextReset", "store::rulesRead", "argumentBuilder::buildArguments")
        }

    @Test
    fun theArgumentsOfTheRulesSupported() {
        val claims = module.values("store::argument(A), store::claim(A, C)", "C").toSet()
        assertEquals(setOf("a", "b", "c"), claims)
    }

    @Test
    fun theSupportsOfTheArguments() {
        val supported = module.values("store::support(S, A), store::claim(S, a), store::claim(A, C)", "C")
        assertEquals(listOf("b"), supported)
    }
}
