package it.unibo.tuprolog.argumentation.core.libs.language

import it.unibo.tuprolog.argumentation.core.ModuleTesting
import kotlin.test.Test
import kotlin.test.assertEquals

class RuleParserTest {
    private fun module() =
        ModuleTesting("r1 : a, ~(b) => c.\nr2 : a => d.\nr3 : p(X) => q(X).").also {
            it.run("context_reset", "parser:::convertAllRules(_)")
        }

    @Test
    fun theRulesUsingAPremiseAreFoundAsByScanningThem() {
        val module = module()
        val scanned = module.values("parser::classic_rule(_, Premises, [S]), member(a, Premises)", "S")
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
}
