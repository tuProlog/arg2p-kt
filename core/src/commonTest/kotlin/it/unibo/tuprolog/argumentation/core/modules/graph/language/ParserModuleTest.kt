package it.unibo.tuprolog.argumentation.core.modules.graph.language

import it.unibo.tuprolog.argumentation.core.ModuleTesting
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertTrue

class ParserModuleTest {
    private fun module() = ModuleTesting.parsed("r1 : a, ~(b) => c.\nr2 : a => d.\nr3 : p(X) => q(X).")

    @Test
    fun theRulesUsingAPremiseAreFoundAsByScanningThem() {
        val module = module()
        val scanned = module.values("theory::rule(_, Premises, [S]), member(a, Premises)", "S")
        assertEquals(scanned, module.values("theory::ruleUsing(a, S)", "S"))
        assertEquals(setOf("c", "d"), scanned.toSet())
    }

    // A stage of a resolution is a call made with :::, and asking the languages of a theory is none
    @Test
    fun theLanguagesAreAskedInTheStageOfTheConversion() {
        val module = ModuleTesting("r1 : [] => a.")
        assertEquals("1", module.value("context_reset, parser:::convertAllRules, context_active(B)", "B"))
        assertEquals("1", module.value("parser::checkLanguages, context_active(B)", "B"))
    }

    // The theory converted once, a solve after the first has the stages of the first
    @Test
    fun theStagesOfASolveAreTheSameAfterTheFirst() {
        val module = ModuleTesting("r1 : [] => a.\nr2 : [] => -a.")
        val first = module.value("buildLabelSetsSilent, context_active(B)", "B")
        assertEquals(first, module.value("buildLabelSetsSilent, context_active(B)", "B"))
    }

    @Test
    fun contraryPremisesAndVariablesAreFound() {
        val module = module()
        assertEquals(listOf("c"), module.values("theory::ruleUsing(~(b), S)", "S"))
        // the conclusion takes the bindings of the premise
        assertEquals(listOf("q(k)"), module.values("theory::ruleUsing(p(k), S)", "S"))
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

    // A premise or an axiom named by a compound term, as a rule may be: an argument each
    @Test
    fun premisesAndAxiomsMayHaveCompoundNames() {
        val statIn = ModuleTesting("f(x) :=> p(x).\ng(y) :-> q(y).\n").value("buildLabelSets(I, _, _)", "I")!!
        assertTrue("[p(x)]" in statIn && "[q(y)]" in statIn, statIn)
    }
}
