package it.unibo.tuprolog.argumentation.core.libraries.framework.parser

import it.unibo.tuprolog.argumentation.core.LibraryTesting
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertTrue

class ParserLibraryTest {
    private fun module() = LibraryTesting.parsed("r1 : a, ~(b) => c.\nr2 : a => d.\nr3 : p(X) => q(X).")

    @Test
    fun theRulesUsingAPremiseAreFoundAsByScanningThem() {
        val module = module()
        val scanned = module.values("store::rule(_, Premises, [S]), member(a, Premises)", "S")
        assertEquals(scanned, module.values("store::ruleUsing(a, S)", "S"))
        assertEquals(setOf("c", "d"), scanned.toSet())
    }

    // A stage of a resolution is a call made with :::, and asking the languages of a theory is none
    @Test
    fun theLanguagesAreAskedInTheActiveStage() {
        val module = LibraryTesting("r1 : [] => a.")
        val active = module.value("context_reset, context_active(B)", "B")
        assertEquals(active, module.value("context_reset, parser::checkLanguages, context_active(B)", "B"))
    }

    // The theory converted once, a solve after the first has the stages of the first
    @Test
    fun theStagesOfASolveAreTheSameAfterTheFirst() {
        val module = LibraryTesting("r1 : [] => a.\nr2 : [] => -a.")
        val first = module.value("solve, context_active(B)", "B")
        assertEquals(first, module.value("solve, context_active(B)", "B"))
    }

    @Test
    fun contraryPremisesAndVariablesAreFound() {
        val module = module()
        assertEquals(listOf("c"), module.values("store::ruleUsing(~(b), S)", "S"))
        // the conclusion takes the bindings of the premise
        assertEquals(listOf("q(k)"), module.values("store::ruleUsing(p(k), S)", "S"))
    }

    // The stated preferences are closed transitively, a cycle making no rule preferred to itself
    // A premise or an axiom named by a compound term, as a rule may be: an argument each
    @Test
    fun premisesAndAxiomsMayHaveCompoundNames() {
        val statIn = LibraryTesting("f(x) :=> p(x).\ng(y) :-> q(y).\n").value("solve(I, _, _)", "I")!!
        assertTrue("[p(x)]" in statIn && "[q(y)]" in statIn, statIn)
    }

    // an extension of the language adds its rules when the flags select it, and is not even loaded otherwise
    @Test
    fun theExtensionsOfTheLanguageAddTheirRulesWhenSelected() {
        assertEquals("[]", LibraryTesting().value("parser::contributions(rules, R)", "R"))
        assertEquals("11", LibraryTesting("temporal.").value("parser::contributions(rules, R), length(R, N)", "N"))
    }
}
