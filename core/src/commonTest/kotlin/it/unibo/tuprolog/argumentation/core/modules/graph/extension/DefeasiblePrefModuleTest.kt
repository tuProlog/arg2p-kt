package it.unibo.tuprolog.argumentation.core.modules.graph.extension

import it.unibo.tuprolog.argumentation.core.ModuleTesting
import kotlin.test.Test
import kotlin.test.assertTrue

class DefeasiblePrefModuleTest {
    // -a rebuts a, and b through it; an argument concludes that r1 (for a) is preferred to r3 (for -a)
    private val theory =
        """
        r1 : [] => a.
        r2 : a => b.
        r3 : [] => -a.
        p : [] => sup(r1, r3).
        graphBuildMode(standard_af).
        statementLabellingMode(statement).
        argumentLabellingMode(grounded).
        orderingPrinciple(last).
        orderingComparator(elitist).
        graphExtension(defeasiblePref).
        """.trimIndent()

    @Test
    fun theAttacksAPreferenceInvalidatesBecomeArgumentsItAttacks() {
        val module = ModuleTesting(theory)
        val statIn = module.value("buildLabelSets(I, _, _)", "I")!!
        val statOut = module.value("buildLabelSets(_, O, _)", "O")!!
        // a and b stand, as the preference defeats the argument for the attack of -a on them
        assertTrue("[a]" in statIn && "[b]" in statIn && "[sup(r1, r3)]" in statIn, statIn)
        assertTrue("['-'(a)]" in statOut && "attack(rebut" in statOut, statOut)
    }

    @Test
    fun theAttackOnASuperArgumentGoesThroughTheAttackArgument() {
        val module = ModuleTesting(theory)
        module.run("buildLabelSets(_, _, _)")
        // the argument for the attack of -a on a also attacks the argument for b, through a
        assertTrue(
            module.holds(
                "defeasiblePref::attackArgument(X, rebut, _, A, A), argument::claim(A, a), " +
                    "argument::claim(B, b), context_check(attack(rebut, X, B, A))",
            ),
        )
        // and the preference attacks the attack argument
        assertTrue(
            module.holds(
                "defeasiblePref::attackArgument(X, rebut, _, _, _), argument::claim(P, sup(r1, r3)), context_check(attack(pref, P, X, X))",
            ),
        )
    }

    private val flags =
        "graphBuildMode(standard_af).\nstatementLabellingMode(statement).\nargumentLabellingMode(grounded).\n" +
            "orderingPrinciple(last).\norderingComparator(elitist).\ngraphExtension(defeasiblePref)."

    // -a, on r2 and r3, rebuts a: sup(r1, r2) or sup(r1, r3) each invalidates the attack, and the first is undercut
    @Test
    fun anAttackStaysInvalidWhileAnyOfItsJustificationsStands() {
        val theory =
            "r1 : [] => a.\nr2 : [] => b.\nr3 : [] => c.\ns1 : b, c -> -a.\np1 : [] => sup(r1, r2).\n" +
                "p2 : [] => sup(r1, r3).\nu : [] => undercut(p1).\n$flags"
        val statIn = ModuleTesting(theory).value("buildLabelSets(I, _, _)", "I")!!
        assertTrue("[a]" in statIn && "[sup(r1, r3)]" in statIn, statIn)
    }

    // d, strict on a premise, is superior to -d (on a defeasible rule) by no preference: -d does not defeat it
    @Test
    fun anAttackInvalidWithoutPreferencesIsRemoved() {
        val module = ModuleTesting("f1 :=> b.\ns1 : b -> d.\nr1 : [] => -d.\n$flags")
        val statIn = module.value("buildLabelSets(I, _, _)", "I")!!
        val statOut = module.value("buildLabelSets(_, O, _)", "O")!!
        assertTrue("[d]" in statIn, statIn)
        assertTrue("['-'(d)]" in statOut, statOut)
    }
}
