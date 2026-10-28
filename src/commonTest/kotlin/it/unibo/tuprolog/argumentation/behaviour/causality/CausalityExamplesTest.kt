package it.unibo.tuprolog.argumentation.behaviour.causality

import it.unibo.tuprolog.argumentation.Arg2p
import kotlin.test.Test

// The causation examples of the literature, by every test: ness/3 holds only for causes with an IN argument
class CausalityExamplesTest {
    @Test
    fun causalityTestRefinedTheory1(): Unit =
        Arg2p
            .of(
                """
                f1 :=> a.
                f1 :=> b.

                r1 : a => c.
                r2 : a, b => c.
                """.trimIndent(),
            ).let { solver ->
                isNessOriginalCause(solver, "a", "c", true)
                isNessOriginalCause(solver, "b", "c", true)

                isNessCause(solver, "a", "c", true)
                isNessCause(solver, "b", "c", true)

                isButForCause(solver, "a", "c")
                isButForCause(solver, "b", "c", false)
            }

    @Test
    fun causalityTestRefinedTheory2(): Unit =
        Arg2p
            .of(
                """
                f1 :=> a.
                f2 :=> b.
                
                r0 : [] => c.
                r1 : -a => -r0.
                r2 : -a, -b => -r0.
                """.trimIndent(),
            ).let { solver ->
                isNessOriginalCause(solver, "a", "c", true)
                isNessOriginalCause(solver, "b", "c", false)

                isNessCause(solver, "a", "c", true)
                isNessCause(solver, "b", "c", true)
            }

    @Test
    fun causalityTestComplex(): Unit =
        Arg2p
            .of(
                """
                tx2 : d, e => undercut(r2).

                r1 :=> a.
                r2 : a => -b.
                """.trimIndent(),
            ).let { solver ->
                isNessOriginalIntervention(solver, "[d]", "-b", false)
                isNessOriginalIntervention(solver, "[d,e]", "-b", true)
                isNessOriginalIntervention(solver, "[d,e,c]", "-b", false)
                isNessOriginalCause(solver, "-d", "-b", true)
                isNessOriginalCause(solver, "b", "a", false)
                isNessOriginalCause(solver, "d", "-b", false)
                isNessOriginalCause(solver, "-e", "-b", true)
                isNessOriginalCause(solver, "a", "-b", true)
                isNessOriginalCause(solver, "r2", "-b", true)

                isNessIntervention(solver, "[d]", "-b", false)
                isNessIntervention(solver, "[d,e]", "-b", true)
                isNessIntervention(solver, "[d,e,c]", "-b", false)
                isNessCause(solver, "-d", "-b", false)
                isNessCause(solver, "b", "a", false)
                isNessCause(solver, "d", "-b", false)
                isNessCause(solver, "-e", "-b", false)
                isNessCause(solver, "a", "-b", true)
                isNessCause(solver, "r2", "-b", false)
            }

    @Test
    fun causalityTest(): Unit =
        Arg2p
            .of(
                """
                tx2 : d => -r2.

                r1 :=> a.
                r2 : a => -b.
                """.trimIndent(),
            ).let { solver ->
                isNessOriginalIntervention(solver, "[-a]", "-b", true)
                isNessOriginalIntervention(solver, "[-b]", "a", false)
                isNessOriginalIntervention(solver, "[-d]", "-b", false)
                isNessOriginalIntervention(solver, "[d]", "-b", true)
                isNessOriginalCause(solver, "a", "-b", true)
                isNessOriginalCause(solver, "b", "a", false)
                isNessOriginalCause(solver, "d", "-b", false)
                isNessOriginalCause(solver, "-d", "-b", true)

                isNessIntervention(solver, "[-a]", "-b", true)
                isNessIntervention(solver, "[-b]", "a", false)
                isNessIntervention(solver, "[-d]", "-b", false)
                isNessIntervention(solver, "[d]", "-b", true)
                isNessCause(solver, "a", "-b", true)
                isNessCause(solver, "b", "a", false)
                isNessCause(solver, "d", "-b", false)
                isNessCause(solver, "-d", "-b", false)
            }

    @Test
    fun asbestos(): Unit =
        Arg2p
            .of(
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
                isNessOriginalIntervention(solver, "[-ex_1]", "di")
                isNessOriginalIntervention(solver, "[-ex_2]", "di")
                isNessOriginalIntervention(solver, "[-ex_3]", "di")

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
        Arg2p
            .of(
                """
                r_0 : ge_di => lu_ca.
                r_1 : ge_di => sm_to.
                
                f_1 :=> ge_di.
                """.trimIndent(),
            ).let { solver ->
                isNessOriginalIntervention(solver, "[-sm_to]", "lu_ca", false)
                isNessOriginalIntervention(solver, "[-ge_di]", "lu_ca")
                isNessOriginalCause(solver, "ge_di", "lu_ca")

                isNessIntervention(solver, "[-sm_to]", "lu_ca", false)
                isNessIntervention(solver, "[-ge_di]", "lu_ca")
                isNessCause(solver, "ge_di", "lu_ca")
            }

    @Test
    fun doubleShooting(): Unit =
        Arg2p
            .of(
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
                isNessOriginalIntervention(solver, "[-bu_sh]", "ge_di")
                isNessOriginalIntervention(solver, "[-dl_sh]", "ge_di", false)

                isNessIntervention(solver, "[-bu_sh]", "ge_di")
                isNessIntervention(solver, "[-dl_sh]", "ge_di", false)
            }

    @Test
    fun leukaemia(): Unit =
        Arg2p
            .of(
                """
                r_0 : ch_le => ch_di.
                r_1 : pa_co => chem.
                r_2 : chem => -r_0.
                
                f_1 :=> ch_le.
                f_2 :=> -pa_co.
                """.trimIndent(),
            ).let { solver ->
                isNessOriginalIntervention(solver, "[-ch_le]", "ch_di")
                isNessOriginalIntervention(solver, "[pa_co]", "ch_di")

                isNessIntervention(solver, "[-ch_le]", "ch_di")
                isNessIntervention(solver, "[pa_co]", "ch_di")
            }

    @Test
    fun brakes(): Unit =
        Arg2p
            .of(
                """
                r_0 : -dr_pu => ac_ha.
                r_1 : br_ma => br_fa.
                r_2 : br_fa => ac_ha.
                r_3 : -dr_pu => -r_1.
                
                f_1 :=> br_ma.
                f_2 :=> -dr_pu.
                """.trimIndent(),
            ).let { solver ->
                isNessOriginalIntervention(solver, "[dr_pu]", "ac_ha")
                isNessOriginalIntervention(solver, "[-br_ma]", "ac_ha", false)

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
            Arg2p.of("$theory\nf1 :=> so_sh.\n").let {
                isNessOriginalIntervention(it, "[-so_sh]", "pr_di")
                isNessOriginalIntervention(it, "[-se_sh]", "pr_di", false)

                isNessIntervention(it, "[-so_sh]", "pr_di")
                isNessIntervention(it, "[-se_sh]", "pr_di", false)
            }
            Arg2p.of("$theory\nf1 :=> -so_sh.\n").let {
                isNessOriginalIntervention(it, "[so_sh]", "pr_di")
                isNessOriginalIntervention(it, "[-se_sh]", "pr_di")

                isNessIntervention(it, "[so_sh]", "pr_di")
                isNessIntervention(it, "[-se_sh]", "pr_di")
            }
        }
}
