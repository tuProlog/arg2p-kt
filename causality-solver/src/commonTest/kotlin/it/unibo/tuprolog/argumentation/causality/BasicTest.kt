package it.unibo.tuprolog.argumentation.causality

import it.unibo.tuprolog.argumentation.core.Arg2pSolverFactory
import it.unibo.tuprolog.argumentation.core.dsl.arg2pScope
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.solve.MutableSolver
import kotlin.test.Test
import kotlin.test.assertEquals

class BasicTest {
    private val temporalDomain = { t: Int ->
        """
        next(N, M) :- integer(N), !, M is N + 1, N >= 0, M =< $t.
        next(N, M) :- integer(M), !, N is M - 1, N >= 0, M =< $t.
        
        nextD(N, M) :- next(N, M).
        nextD(N, M) :- integer(N), !, next(N, M1), nextD(M1, M).
        nextD(N, M) :- integer(M), !, next(N1, M), nextD(N, N1).
        """.trimIndent()
    }

    private fun isNessOldIntervention(
        solver: MutableSolver,
        cause: String,
        effect: String,
        outcome: Boolean = true,
    ) = arg2pScope {
        solver.solve("context_reset" and "ness_original_intervention"(Term.parse(cause), effect)).first().also {
            assertEquals(it.isYes, outcome)
        }
    }

    private fun isNessOldCause(
        solver: MutableSolver,
        cause: String,
        effect: String,
        outcome: Boolean = true,
    ) = arg2pScope {
        solver.solve("context_reset" and "ness_original"(X, Term.parse(cause), effect)).first().also {
            assertEquals(it.isYes, outcome)
            println(it.substitution[X])
        }
    }

    private fun isNessIntervention(
        solver: MutableSolver,
        cause: String,
        effect: String,
        outcome: Boolean = true,
    ) = arg2pScope {
        solver.solve("context_reset" and "ness_intervention"(Term.parse(cause), effect)).first().also {
            assertEquals(it.isYes, outcome)
        }
    }

    private fun isNessCause(
        solver: MutableSolver,
        cause: String,
        effect: String,
        outcome: Boolean = true,
    ) = arg2pScope {
        solver.solve("context_reset" and "ness"(X, Term.parse(cause), effect)).first().also {
            assertEquals(it.isYes, outcome)
            println(it.substitution[X])
        }
    }

    private fun isButForCause(
        solver: MutableSolver,
        cause: String,
        effect: String,
        outcome: Boolean = true,
    ) = arg2pScope {
        solver.solve("context_reset" and "but_for"(Term.parse(cause), effect)).first().also {
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
    fun causalityTestRefinedTheory1(): Unit =
        Arg2pSolverFactory
            .causality(
                """
                f1 :=> a.
                f1 :=> b.

                r1 : a => c.
                r2 : a, b => c.
                """.trimIndent(),
            ).let { solver ->
                isNessOldCause(solver, "a", "c", true)
                isNessOldCause(solver, "b", "c", true)

                isNessCause(solver, "a", "c", true)
                isNessCause(solver, "b", "c", true)

                isButForCause(solver, "a", "c")
                isButForCause(solver, "b", "c", false)
            }

    @Test
    fun causalityTestRefinedTheory2(): Unit =
        Arg2pSolverFactory
            .causality(
                """
                f1 :=> a.
                f2 :=> b.
                
                r0 : [] => c.
                r1 : -a => -r0.
                r2 : -a, -b => -r0.
                """.trimIndent(),
            ).let { solver ->
                isNessOldCause(solver, "a", "c", true)
                isNessOldCause(solver, "b", "c", false)

                isNessCause(solver, "a", "c", true)
                isNessCause(solver, "b", "c", true)
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
                isNessOldIntervention(solver, "[d]", "-b", false)
                isNessOldIntervention(solver, "[d,e]", "-b", true)
                isNessOldIntervention(solver, "[d,e,c]", "-b", false)
                isNessOldCause(solver, "-d", "-b", true)
                isNessOldCause(solver, "b", "a", false)
                isNessOldCause(solver, "d", "-b", false)
                isNessOldCause(solver, "-e", "-b", true)
                isNessOldCause(solver, "a", "-b", true)
                isNessOldCause(solver, "r2", "-b", true)

                isNessIntervention(solver, "[d]", "-b", false)
                isNessIntervention(solver, "[d,e]", "-b", true)
                isNessIntervention(solver, "[d,e,c]", "-b", false)
                isNessCause(solver, "-d", "-b", true)
                isNessCause(solver, "b", "a", false)
                isNessCause(solver, "d", "-b", false)
                isNessCause(solver, "-e", "-b", true)
                isNessCause(solver, "a", "-b", true)
                isNessCause(solver, "r2", "-b", true)
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
                isNessOldIntervention(solver, "[-a]", "-b", true)
                isNessOldIntervention(solver, "[-b]", "a", false)
                isNessOldIntervention(solver, "[-d]", "-b", false)
                isNessOldIntervention(solver, "[d]", "-b", true)
                isNessOldCause(solver, "a", "-b", true)
                isNessOldCause(solver, "b", "a", false)
                isNessOldCause(solver, "d", "-b", false)
                isNessOldCause(solver, "-d", "-b", true)

                isNessIntervention(solver, "[-a]", "-b", true)
                isNessIntervention(solver, "[-b]", "a", false)
                isNessIntervention(solver, "[-d]", "-b", false)
                isNessIntervention(solver, "[d]", "-b", true)
                isNessCause(solver, "a", "-b", true)
                isNessCause(solver, "b", "a", false)
                isNessCause(solver, "d", "-b", false)
                isNessCause(solver, "-d", "-b", true)
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
                isNessOldIntervention(solver, "[-ex_1]", "di")
                isNessOldIntervention(solver, "[-ex_2]", "di")
                isNessOldIntervention(solver, "[-ex_3]", "di")

                isNessIntervention(solver, "[-ex_1]", "di")
                isNessIntervention(solver, "[-ex_2]", "di")
                isNessIntervention(solver, "[-ex_3]", "di")

                isNessCause(solver, "ex_1", "di")
                isNessCause(solver, "ex_2", "di")
                isNessCause(solver, "ex_3", "di")

                isButForCause(solver, "ex_1", "di", false)
                isButForCause(solver, "ex_2", "di", false)
                isButForCause(solver, "ex_3", "di", false)
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
                isNessOldIntervention(solver, "[-sm_to]", "lu_ca", false)
                isNessOldIntervention(solver, "[-ge_di]", "lu_ca")
                isNessOldCause(solver, "ge_di", "lu_ca")

                isNessIntervention(solver, "[-sm_to]", "lu_ca", false)
                isNessIntervention(solver, "[-ge_di]", "lu_ca")
                isNessCause(solver, "ge_di", "lu_ca")
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
                isNessOldIntervention(solver, "[-bu_sh]", "ge_di")
                isNessOldIntervention(solver, "[-dl_sh]", "ge_di", false)

                isNessIntervention(solver, "[-bu_sh]", "ge_di")
                isNessIntervention(solver, "[-dl_sh]", "ge_di", false)
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
                isNessOldIntervention(solver, "[-ch_le]", "ch_di")
                isNessOldIntervention(solver, "[pa_co]", "ch_di")

                isNessIntervention(solver, "[-ch_le]", "ch_di")
                isNessIntervention(solver, "[pa_co]", "ch_di")
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
                isNessOldIntervention(solver, "[dr_pu]", "ac_ha")
                isNessOldIntervention(solver, "[-br_ma]", "ac_ha", false)

                isNessIntervention(solver, "[dr_pu]", "ac_ha")
                isNessIntervention(solver, "[-br_ma]", "ac_ha", false)
            }

    @Test
    fun soldiers(): Unit =
        """
        r_0 : so_sh => pr_di.
        r_1 : se_sh => pr_di.
        r_2 : -so_sh => se_sh.
        """.trimIndent().let { theory ->
            Arg2pSolverFactory.causality("$theory\nf1 :=> so_sh.\n").let {
                isNessOldIntervention(it, "[-so_sh]", "pr_di")
                isNessOldIntervention(it, "[-se_sh]", "pr_di", false)

                isNessIntervention(it, "[-so_sh]", "pr_di")
                isNessIntervention(it, "[-se_sh]", "pr_di", false)
            }
            Arg2pSolverFactory.causality("$theory\nf1 :=> -so_sh.\n").let {
                isNessOldIntervention(it, "[so_sh]", "pr_di")
                isNessOldIntervention(it, "[-se_sh]", "pr_di")

                isNessIntervention(it, "[so_sh]", "pr_di")
                isNessIntervention(it, "[-se_sh]", "pr_di")
            }
        }

    @Test
    fun doubleShootingTemporal(): Unit =
        Arg2pSolverFactory
            .causality(
                """
                r_1(X, Y, T) : sh(X, Y, T), prolog(next(T, T1)) => hit(X, Y, T1).
                r_2(X, Y, T) : hit(X, Y, T) => di(Y, T).
                r_3(X, Z, Y, T1, T2) : hit(X, Y, T1), hit(Z, Y, T2), prolog(nextD(T1, T2)) => -r_2(Z, Y, T2).
                
                f_1 :=> sh(bu, ge, 1).
                f_2 :=> sh(dl, ge, 3).
                
                """.trimIndent() + temporalDomain(5),
            ).let { solver ->
                isNessOldIntervention(solver, "[-sh(bu, ge, 1)]", "di(ge, 2)")
                isNessOldIntervention(solver, "[-sh(dl, ge, 3)]", "di(ge, 2)", false)
                isNessOldIntervention(solver, "[-sh(dl, ge, 3)]", "di(ge, 4)", false)

                isNessIntervention(solver, "[-sh(bu, ge, 1)]", "di(ge, 2)")
                isNessIntervention(solver, "[-sh(dl, ge, 3)]", "di(ge, 2)", false)
                isNessIntervention(solver, "[-sh(dl, ge, 3)]", "di(ge, 4)", false)
            }

    @Test
    fun doubleShootingTemporalAdvanced(): Unit =
        Arg2pSolverFactory
            .causality(
                """
                r1(X, Y, T) : causesF(X, Y), happens(X, T) => holdsAt(Y, T).
                r2(X, Y, T) : causesE(X, Y), happens(X, T), prolog(next(T, T1)) => happens(Y, T1).
                r1_u(Y, T) : holdsAt(Y, T), prolog(next(T, T1)) => -r1(_, Y, T1).
                
                ri(Y, T) : holdsAt(Y, T), prolog(next(T, T1)) => holdsAt(Y, T1).
                
                d1 :=> causesE(sh(X, Y), hit(X, Y)).
                d2 :=> causesF(hit(X, Y), dead(Y)).
                
                f_1 :=> happens(sh(bu, ge), 1).
                f_2 :=> happens(sh(dl, ge), 3).
                
                """.trimIndent() + temporalDomain(5),
            ).let { solver ->
                isNessOldIntervention(solver, "[-happens(sh(bu, ge), 1)]", "holdsAt(dead(ge), 5)")
                isNessOldIntervention(solver, "[-happens(sh(bu, ge), 3)]", "holdsAt(dead(ge), 5)", false)

                isNessIntervention(solver, "[-happens(sh(bu, ge), 1)]", "holdsAt(dead(ge), 5)")
                isNessIntervention(solver, "[-happens(sh(bu, ge), 3)]", "holdsAt(dead(ge), 5)", false)
            }

    @Test
    fun poisoningTemporal() {
        Arg2pSolverFactory
            .causality(
                """
                r1(X, Y, T) : happens(X, T), causesF(X, Y, T)  => holdsAt(Y, T).
                r2(X, Y, T) : happens(X, T), causesE(X, Y, T), prolog(next(T, T1)) => happens(Y, T1).
                r3(Y, T) : holdsAt(Y, T), prolog(next(T, T1)) => -r1(_, Y, T1).
                r4(Y, T) : holdsAt(Y, T), prolog(next(T, T1)) => holdsAt(Y, T1).
                r5(Y, T) : happens(X, T), causesF(X, Y, T), prolog((complement(Y, CY), next(T1, T))) => -r4(CY, T1).
                s1(Y, T) : holdsAt(Y, T), prolog(complement(Y, CY))  -> -holdsAt(CY, T).

                complement(-X, X).
                complement(X, -X) :- X \= -_.

                c1 :=> causesF(empties, empty, T).
                c2 :=> causesF(poisons, poisoned, T).
                c3(T) : holdsAt(-empty, T), holdsAt(poisoned, T) => causesE(thirst, drinksPoison, T).
                c4(T) : holdsAt(empty, T) => causesE(thirst, dehydration, T).
                c5 :=> causesF(drinksPoison, dead, T).
                c6 :=> causesF(dehydration, dead, T).

                f_1 :=> holdsAt(-empty, 0).
                f_2 :=> happens(poisons, 1).
                f_3 :=> happens(empties, 2).
                f_4 :=> happens(thirst, 4).
                
                """.trimIndent() + temporalDomain(5),
            ).let { solver ->
                isNessOldIntervention(solver, "[-happens(poisons, 1)]", "holdsAt(dead, 5)", false)
                isNessOldIntervention(solver, "[-happens(empties, 2)]", "holdsAt(dead, 5)")

                isNessIntervention(solver, "[-happens(poisons, 1)]", "holdsAt(dead, 5)", false)
                isNessIntervention(solver, "[-happens(empties, 2)]", "holdsAt(dead, 5)")
            }
    }
}
