package it.unibo.tuprolog.argumentation.core.engine

import it.unibo.tuprolog.argumentation.core.Arg2pSolverFactory
import it.unibo.tuprolog.argumentation.core.runtime.settings.FlagsBuilder
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.parsing.parse
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertTrue

// The theory of the user is a module of its own: the modules reach it through user:: alone, and its predicates are its
// own, whatever the libraries and the modules name theirs
class UserModuleTest {
    private fun solver(theory: String) = Arg2pSolverFactory.default(theory = theory, settings = FlagsBuilder().create())

    private fun statementsIn(theory: String): String {
        val solver = solver(theory)
        return solver
            .solve(Struct.parse("buildLabelSets(I, _, _)", solver.operators))
            .first()
            .substitution
            .getByName("I")
            .toString()
    }

    @Test
    fun aGoalOfTheUserIsSolvedInTheModuleUser() {
        val solver = solver("p(a).\np(b).\n")
        val solution = solver.solve(Struct.parse("findall(X, user::p(X), L)", solver.operators)).first()
        assertEquals("[a, b]", solution.substitution.getByName("L").toString())
    }

    // member/2 of the theory is the one its checks call, not the one of the libraries
    @Test
    fun aPredicateOfTheTheoryIsItsOwnWhateverTheLibrariesDefine() {
        val checks = "r1 : prolog(member(y, [y])) => a.\nr2 : prolog(member(x, [y])) => b.\n"
        assertTrue("a" in statementsIn(checks))
        val own = statementsIn(checks + "member(x, _).\n")
        assertFalse("a" in own, own)
        assertTrue("b" in own, own)
    }

    // The modules have predicates named contrary/2, declared/2, holds/1, ...: those of the theory change nothing in them
    @Test
    fun theModulesDoNotSeeThePredicatesOfTheTheory() {
        val theory = "f1 :-> b.\ns1 : b -> c.\nr1 : [] => d.\ncontrary(d, c).\ndeclared(d, c).\nstrictly(c).\nholds(x).\n"
        assertTrue("d" in statementsIn(theory))
    }
}
