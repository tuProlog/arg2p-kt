package it.unibo.tuprolog.argumentation.core.libraries.graph.structured.extension.rebutRestriction

import it.unibo.tuprolog.argumentation.Flags
import it.unibo.tuprolog.argumentation.core.Arg2pSolverFactory
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import kotlin.test.Test
import kotlin.test.assertEquals

// The restricted rebut of ASPIC+: no rebut on the conclusion of a strict rule
class RebutRestrictionLibraryTest {
    // b is concluded by a strict rule on a defeasible one (a rebut is on an argument with a defeasible rule), -b by a
    // defeasible rule
    private val theory = "f1 :=> a.\nr0 : a => d.\ns1 : d -> b.\nf2 :=> c.\nr1 : c => -b.\nqueryMode(off).\n"

    // The rebuttals of the argument for b, once the graph is changed by the step stated alone
    private fun rebuttalsOfB(step: String): String {
        val solver = Arg2pSolverFactory.default(theory = theory + "graphExtension($step).\n", settings = Flags.defaults().library())
        val goal = "solve(_, _, _), findall(A, (store::directAttack(rebut, A, B), store::claim(B, b)), As), length(As, N)"
        return solver
            .solve(Struct.parse(goal, solver.operators))
            .first()
            .substitution
            .getByName("N")
            .toString()
    }

    @Test
    fun aStrictConclusionIsNotRebutted() {
        assertEquals("0", rebuttalsOfB("rebutRestriction"))
    }

    @Test
    fun withoutTheRestrictionItIs() {
        assertEquals("1", rebuttalsOfB("none"))
    }
}
