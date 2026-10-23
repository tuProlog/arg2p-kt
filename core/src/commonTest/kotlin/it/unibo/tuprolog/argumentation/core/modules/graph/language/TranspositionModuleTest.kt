package it.unibo.tuprolog.argumentation.core.modules.graph.language

import it.unibo.tuprolog.argumentation.core.ModuleTesting
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.parsing.parse
import kotlin.test.Test
import kotlin.test.assertEquals

class TranspositionModuleTest {
    private val rules = "[[s1, (a, b), c]]"

    @Test
    fun eachPremiseIsConcludedNegatedFromTheNegatedConclusion() {
        val module = ModuleTesting("autoTransposition.")
        val transposed =
            module
                .value("transposition::transpose($rules, $rules, T)", "T")!!
                .let { Term.parse(it, module.solver.operators) }
                .castToList()
                .toList()
                .toSet()
        assertEquals(
            setOf("[s1, (a, b), c]", "[s1_i, ('-'(c), b), '-'(a)]", "[s1_ii, ('-'(c), a), '-'(b)]").map { Term.parse(it) }.toSet(),
            transposed,
        )
    }

    @Test
    fun withoutAutoTranspositionTheRulesStayAsTheyAre() {
        val module = ModuleTesting()
        assertEquals(Term.parse(rules), Term.parse(module.value("transposition::transpose($rules, $rules, T)", "T")!!))
    }

    // Matched as they are: -X, the permissions p/1 and nothing else
    @Test
    fun negations() {
        val module = ModuleTesting()
        assertEquals(
            listOf("a", "-(p)", "o(-(a))", "o(a)", "-(p(a, b))", "-(a - b)").map { Term.parse(it) },
            listOf("-(a)", "p", "p(a)", "p(-(a))", "p(a, b)", "a - b").map {
                Term.parse(module.value("transposition::negate($it, N)", "N")!!)
            },
        )
    }
}
