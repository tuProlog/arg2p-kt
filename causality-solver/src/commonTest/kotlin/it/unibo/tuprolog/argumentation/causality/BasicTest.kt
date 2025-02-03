package it.unibo.tuprolog.argumentation.causality

import it.unibo.tuprolog.argumentation.core.Arg2pSolverFactory
import it.unibo.tuprolog.argumentation.core.dsl.arg2pScope
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.solve.MutableSolver
import kotlin.test.Test
import kotlin.test.assertEquals

class BasicTest {
    private fun isCause(
        solver: MutableSolver,
        cause: String,
        effect: String,
        outcome: Boolean = true,
    ) = arg2pScope {
        solver.solve("solve"(Term.parse(cause), effect)).first().also {
            assertEquals(it.isYes, outcome)
        }
    }

    @Test
    fun causalityTest(): Unit =
        Arg2pSolverFactory.causality(
            """
            tx2 : d => undercut(r2).

            r1 :=> a.
            r2 : a => -b.
            """.trimIndent(),
        ).let { solver ->
            isCause(solver, "[-a]", "-b", true)
            isCause(solver, "[-b]", "a", false)
            isCause(solver, "[-d]", "-b", false)
            isCause(solver, "[d]", "-b", true)
        }

    @Test
    fun asbestos(): Unit =
        Arg2pSolverFactory.causality(
            """
            r_0 : ca => di.
            r_1 : ex_1, ex_2 => ca.
            r_2 : ex_2, ex_3 => ca.
            r_3 : ex_1, ex_3 => ca.
            
            f_1 :=> ex_1.
            f_2 :=> ex_2.
            f_3 :=> ex_3.
            """.trimIndent(),
        ).let { solver ->
            isCause(solver, "[-ex_1]", "di")
            isCause(solver, "[-ex_2]", "di")
            isCause(solver, "[-ex_3]", "di")
        }

    @Test
    fun tobacco(): Unit =
        Arg2pSolverFactory.causality(
            """
            r_0 : ge_di => lu_ca.
            r_1 : ge_di => sm_to.
            
            f_1 :=> ge_di.
            """.trimIndent(),
        ).let { solver ->
            isCause(solver, "[-sm_to]", "lu_ca", false)
        }

    @Test
    fun doubleShooting(): Unit =
        Arg2pSolverFactory.causality(
            """
            r_0 : bu_sh => bu_ki.
            r_1 : dl_sh => du_ki.
            r_2 : bu_ki => ge_di.
            r_3 : du_ki => ge_di.
            r_4 : bu_ki => undercut(r_1).
            
            f_1 :=> bu_sh.
            f_1 :=> dl_sh.
            """.trimIndent(),
        ).let { solver ->
            isCause(solver, "[-bu_sh]", "ge_di")
            isCause(solver, "[-dl_sh]", "ge_di", false)
        }

    @Test
    fun leukaemia(): Unit =
        Arg2pSolverFactory.causality(
            """
            r_0 : ch_le => ch_di.
            r_1 : pa_co => chem.
            r_2 : chem => undercut(r_0).
            
            f_1 :=> ch_le.
            f_2 :=> -pa_co.
            """.trimIndent(),
        ).let { solver ->
            isCause(solver, "[-ch_le]", "ch_di")
            isCause(solver, "[pa_co]", "ch_di")
        }

    @Test
    fun brakes(): Unit =
        Arg2pSolverFactory.causality(
            """
            r_0 : -dr_pu => ac_ha.
            r_1 : br_ma => br_fa.
            r_2 : br_fa => ac_ha.
            r_3 : -dr_pu => undercut(r_1).
            
            f_1 :=> br_ma.
            f_2 :=> -dr_pu.
            """.trimIndent(),
        ).let { solver ->
            isCause(solver, "[dr_pu]", "ac_ha")
            isCause(solver, "[-br_ma]", "ac_ha", false)
        }

    @Test
    fun soldiers(): Unit =
        """
        r_0 : so_sh => pr_di.
        r_1 : se_sh => pr_di.
        r_2 : -so_sh => se_sh.
        """.trimIndent().let { theory ->
            Arg2pSolverFactory.causality("$theory\nf1 :=> so_sh.\n").let {
                isCause(it, "[-so_sh]", "pr_di")
                isCause(it, "[-se_sh]", "pr_di", false)
            }
            Arg2pSolverFactory.causality("$theory\nf1 :=> -so_sh.\n").let {
                isCause(it, "[so_sh]", "pr_di")
                isCause(it, "[-se_sh]", "pr_di")
            }
        }

    @Test
    fun doubleShootingTemporal(): Unit =
        Arg2pSolverFactory.causality(
            """
            r_1(X, Y, T) : sh(X, Y, T), prolog(T1 is T + 1) => hit(X, Y, T1).
            r_2(X, Y, T) : hit(X, Y, T) => di(Y, T).
            r_3(X, Z, Y, T1, T2) : hit(X, Y, T1), hit(Z, Y, T2), prolog((X \= Z, T1 < T2)) => undercut(r_2(Z, Y, T2)).
            
            f_1 :=> sh(bu, ge, 1).
            f_2 :=> sh(dl, ge, 3).
            """.trimIndent(),
        ).let { solver ->
            isCause(solver, "[-sh(bu, ge, 1)]", "di(ge, 2)")
            isCause(solver, "[-sh(dl, ge, 3)]", "di(ge, 2)", false)
            isCause(solver, "[-sh(dl, ge, 3)]", "di(ge, 4)", false)
        }
}
