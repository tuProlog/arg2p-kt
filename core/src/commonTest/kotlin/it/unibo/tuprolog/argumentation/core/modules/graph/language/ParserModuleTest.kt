package it.unibo.tuprolog.argumentation.core.modules.graph.language

import it.unibo.tuprolog.argumentation.core.ModuleTesting
import kotlin.test.Test
import kotlin.test.assertEquals

class ParserModuleTest {
    private fun module() =
        ModuleTesting("r1 : a, ~(b) => c.\nr2 : a => d.\nr3 : p(X) => q(X).").also {
            it.run("context_reset", "parser:::convertAllRules(_)")
        }

    @Test
    fun theRulesUsingAPremiseAreFoundAsByScanningThem() {
        val module = module()
        val scanned = module.values("parser::classicRule(_, Premises, [S]), member(a, Premises)", "S")
        assertEquals(scanned, module.values("parser::ruleUsing(a, S)", "S"))
        assertEquals(setOf("c", "d"), scanned.toSet())
    }

    @Test
    fun contraryPremisesAndVariablesAreFound() {
        val module = module()
        assertEquals(listOf("c"), module.values("parser::ruleUsing(~(b), S)", "S"))
        // the conclusion takes the bindings of the premise
        assertEquals(listOf("q(k)"), module.values("parser::ruleUsing(p(k), S)", "S"))
    }

    // The stated preferences are closed transitively, a cycle making no rule preferred to itself
    @Test
    fun theStatedPreferencesAreClosedTransitively() {
        val module =
            ModuleTesting("sup(r1, r2).\nsup(r2, r3).\nsup(r3, r4).\nsup(s1, s2).\nsup(s2, s1).").also {
                it.run("context_reset", "parser:::convertAllRules(_)")
            }
        assertEquals(
            setOf("r1, r2", "r2, r3", "r3, r4", "r1, r3", "r2, r4", "r1, r4", "s1, s2", "s2, s1").map { "sup($it)" }.toSet(),
            module.values("S = sup(_, _), context_check(S)", "S").toSet(),
        )
    }
}
