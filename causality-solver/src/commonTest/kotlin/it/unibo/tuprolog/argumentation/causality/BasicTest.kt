package it.unibo.tuprolog.argumentation.causality

import it.unibo.tuprolog.argumentation.core.Arg2pSolverFactory
import it.unibo.tuprolog.argumentation.core.dsl.arg2pScope
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.solve.MutableSolver
import kotlin.test.Test
import kotlin.test.assertEquals

class BasicTest {
    private fun isIntervention(
        solver: MutableSolver,
        cause: String,
        effect: String,
        outcome: Boolean = true,
    ) = arg2pScope {
        solver.solve("context_reset" and "evaluate_intervention"(Term.parse(cause), effect)).first().also {
            assertEquals(it.isYes, outcome)
        }
    }

    private fun isCause(
        solver: MutableSolver,
        cause: String,
        effect: String,
        outcome: Boolean = true,
    ) = arg2pScope {
        solver.solve("context_reset" and "evaluate"(X, Term.parse(cause), effect)).first().also {
            assertEquals(it.isYes, outcome)
            println(it.substitution[X])
        }
    }

    @Test
    fun testSubset(): Unit =
        Arg2pSolverFactory.causality().let { solver ->
            arg2pScope {
                solver.solve("proper_subsets"(Term.parse("[a, b, c]"), X)).filter { it.isYes }.forEach {
                    println(it.substitution[X])
                }
            }
        }

    @Test
    fun causalityTestComplex(): Unit =
        Arg2pSolverFactory
            .causality(
                """
                tx2 : d, e => undercut(r2).

                r1 :=> a.
                r2 : a => -b.
                """.trimIndent(),
            ).let { solver ->
                isIntervention(solver, "[d]", "-b", false)
                isIntervention(solver, "[d,e]", "-b", true)
                isIntervention(solver, "[d,e,c]", "-b", false)
                isCause(solver, "-d", "-b", true)
                isCause(solver, "b", "a", false)
                isCause(solver, "d", "-b", false)
                isCause(solver, "-e", "-b", true)
                isCause(solver, "a", "-b", true)
                isCause(solver, "r2", "-b", true)
            }

    @Test
    fun causalityTest(): Unit =
        Arg2pSolverFactory
            .causality(
                """
                tx2 : d => -r2.

                r1 :=> a.
                r2 : a => -b.
                """.trimIndent(),
            ).let { solver ->
                isIntervention(solver, "[-a]", "-b", true)
                isIntervention(solver, "[-b]", "a", false)
                isIntervention(solver, "[-d]", "-b", false)
                isIntervention(solver, "[d]", "-b", true)
                isCause(solver, "a", "-b", true)
                isCause(solver, "b", "a", false)
                isCause(solver, "d", "-b", false)
                isCause(solver, "-d", "-b", true)
            }

    @Test
    fun asbestos(): Unit =
        Arg2pSolverFactory
            .causality(
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
                isIntervention(solver, "[-ex_1]", "di")
                isIntervention(solver, "[-ex_2]", "di")
                isIntervention(solver, "[-ex_3]", "di")
            }

    @Test
    fun tobacco(): Unit =
        Arg2pSolverFactory
            .causality(
                """
                r_0 : ge_di => lu_ca.
                r_1 : ge_di => sm_to.
                
                f_1 :=> ge_di.
                """.trimIndent(),
            ).let { solver ->
                isIntervention(solver, "[-sm_to]", "lu_ca", false)
                isIntervention(solver, "[-ge_di]", "lu_ca")
                isCause(solver, "ge_di", "lu_ca")
            }

    @Test
    fun doubleShooting(): Unit =
        Arg2pSolverFactory
            .causality(
                """
                r_0 : bu_sh => bu_ki.
                r_1 : dl_sh => du_ki.
                r_2 : bu_ki => ge_di.
                r_3 : du_ki => ge_di.
                r_4 : bu_ki => -r_1.
                
                f_1 :=> bu_sh.
                f_1 :=> dl_sh.
                """.trimIndent(),
            ).let { solver ->
                isIntervention(solver, "[-bu_sh]", "ge_di")
                isIntervention(solver, "[-dl_sh]", "ge_di", false)
            }

    @Test
    fun leukaemia(): Unit =
        Arg2pSolverFactory
            .causality(
                """
                r_0 : ch_le => ch_di.
                r_1 : pa_co => chem.
                r_2 : chem => -r_0.
                
                f_1 :=> ch_le.
                f_2 :=> -pa_co.
                """.trimIndent(),
            ).let { solver ->
                isIntervention(solver, "[-ch_le]", "ch_di")
                isIntervention(solver, "[pa_co]", "ch_di")
            }

    @Test
    fun brakes(): Unit =
        Arg2pSolverFactory
            .causality(
                """
                r_0 : -dr_pu => ac_ha.
                r_1 : br_ma => br_fa.
                r_2 : br_fa => ac_ha.
                r_3 : -dr_pu => -r_1.
                
                f_1 :=> br_ma.
                f_2 :=> -dr_pu.
                """.trimIndent(),
            ).let { solver ->
                isIntervention(solver, "[dr_pu]", "ac_ha")
                isIntervention(solver, "[-br_ma]", "ac_ha", false)
            }

    @Test
    fun soldiers(): Unit =
        """
        r_0 : so_sh => pr_di.
        r_1 : se_sh => pr_di.
        r_2 : -so_sh => se_sh.
        """.trimIndent().let { theory ->
            Arg2pSolverFactory.causality("$theory\nf1 :=> so_sh.\n").let {
                isIntervention(it, "[-so_sh]", "pr_di")
                isIntervention(it, "[-se_sh]", "pr_di", false)
            }
            Arg2pSolverFactory.causality("$theory\nf1 :=> -so_sh.\n").let {
                isIntervention(it, "[so_sh]", "pr_di")
                isIntervention(it, "[-se_sh]", "pr_di")
            }
        }

    @Test
    fun doubleShootingTemporal(): Unit =
        Arg2pSolverFactory
            .causality(
                """
                r_1(X, Y, T) : sh(X, Y, T), prolog(T1 is T + 1) => hit(X, Y, T1).
                r_2(X, Y, T) : hit(X, Y, T) => di(Y, T).
                r_3(X, Z, Y, T1, T2) : hit(X, Y, T1), hit(Z, Y, T2), prolog((X \= Z, T1 < T2)) => -r_2(Z, Y, T2).
                
                f_1 :=> sh(bu, ge, 1).
                f_2 :=> sh(dl, ge, 3).
                """.trimIndent(),
            ).let { solver ->
                isIntervention(solver, "[-sh(bu, ge, 1)]", "di(ge, 2)")
                isIntervention(solver, "[-sh(dl, ge, 3)]", "di(ge, 2)", false)
                isIntervention(solver, "[-sh(dl, ge, 3)]", "di(ge, 4)", false)
            }

    @Test
    fun doubleShootingTemporalAdvanced(): Unit =
        Arg2pSolverFactory
            .causality(
                """
                r1(X, Y, T) : causesF(X, Y), happens(X, T) => holdsAt(Y, T).
                r2(X, Y, T) : causesE(X, Y), happens(X, T), prolog(T1 is T + 1) => happens(Y, T1).
                r1_u(Y, T) : holdsAt(Y, T), prolog(T1 is T + 1) => -r1(_, Y, T1).
                
                ri(Y, T) : holdsAt(Y, T), prolog((T1 is T + 1, T < 5)) => holdsAt(Y, T1).
                
                d1 :=> causesE(sh(X, Y), hit(X, Y)).
                d2 :=> causesF(hit(X, Y), dead(Y)).
                
                f_1 :=> happens(sh(bu, ge), 1).
                f_2 :=> happens(sh(dl, ge), 3).
                """.trimIndent(),
            ).let { solver ->
                isIntervention(solver, "[-happens(sh(bu, ge), 1)]", "holdsAt(dead(ge), 5)")
                isIntervention(solver, "[-happens(sh(bu, ge), 3)]", "holdsAt(dead(ge), 5)", false)
            }
}
