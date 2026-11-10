package it.unibo.tuprolog.argumentation.core.libraries.framework.store

import it.unibo.tuprolog.argumentation.core.LibraryTesting
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertTrue

class StoreLibraryTest {
    private val a = "[[r1], r1, [a], [], [[r1], [r1], []]]"
    private val b = "[[r2, r1], r2, [b], [a], [[r2], [r2, r1], []]]"
    private val c = "[[r3], r3, [c], [], [[r3], [r3], []]]"

    @Test
    fun anArgumentIsSavedWithItsLookups() {
        val module = LibraryTesting()
        module.run("store::saveArgument(tag, [a], $a)")
        assertTrue(module.holds("context_check(argument($a))"))
        assertTrue(module.holds("store::argumentFor([a], X), X == $a"))
        assertTrue(module.holds("context_id(argument($a), Id), store::argumentById(Id, X), X == $a"))
        assertTrue(
            module.holds("context_id(argument($a), Id), store::argumentInfoId(Id, K), store::infoById(K, [[r1], [r1], []])"),
        )
        assertTrue(module.holds("context_check(newConc(tag, [a]))"))
    }

    @Test
    fun anArgumentIsSavedOnce() {
        val module = LibraryTesting()
        module.run("store::saveArgument(tag, [a], $a)")
        assertFalse(module.holds("store::saveArgument(tag, [a], $a)"))
        assertEquals(1, module.solutions("context_check(argument(X))").count { it.isYes })
    }

    @Test
    fun aSupportIsSavedByArgumentsAndIdentifiers() {
        val module = LibraryTesting()
        module.run("store::saveArgument(t, [a], $a)", "store::saveArgument(t, [b], $b)", "store::saveSupport($a, $b)")
        assertTrue(module.holds("context_check(support($a, $b))"))
        assertTrue(module.holds("context_id(argument($a), IdA), context_id(argument($b), IdB), context_check(supportId(IdA, IdB))"))
        // saved already, it still succeeds, and the support stays one: a rule may use an argument for two premises
        assertTrue(module.holds("store::saveSupport($a, $b)"))
        assertEquals("1", module.value("findall(I, context_check(supportId(_, I)), L), length(L, N)", "N"))
    }

    @Test
    fun anAttackIsSavedAndRemovedInAllItsForms() {
        val module = LibraryTesting()
        module.run(
            "store::saveArgument(t, [a], $a)",
            "store::saveArgument(t, [b], $b)",
            "store::saveArgument(t, [c], $c)",
            "store::saveAttack(rebut, $c, $b, $a)",
        )
        val ids = "context_id(argument($c), IdC), context_id(argument($b), IdB), context_id(argument($a), IdA)"
        assertTrue(module.holds("context_check(attack(rebut, $c, $b, $a))"))
        assertTrue(module.holds("$ids, context_check(attackId(rebut, IdC, IdB, IdA))"))
        assertFalse(module.holds("store::saveAttack(rebut, $c, $b, $a)"))

        module.run("store::removeAttack(rebut, $c, $b, $a)")
        assertFalse(module.holds("context_check(attack(_, _, _, _))"))
        assertFalse(module.holds("context_check(attackId(_, _, _, _))"))
    }

    @Test
    fun argumentsAreLabelledByThemselvesAndByIdentifiers() {
        val module = LibraryTesting()
        module.run("store::saveArgument(t, [a], $a)", "store::argumentId($a, Id), store::labelArgument(und, $a, Id)")
        assertTrue(module.holds("store::argumentLabel(und, $a)"))
        assertTrue(module.holds("store::argumentId($a, Id), store::argumentLabelById(und, Id)"))
        module.run("store::unlabelArguments(und)")
        assertFalse(module.holds("store::argumentLabel(_, _)"))
        assertFalse(module.holds("store::argumentLabelById(_, _)"))
    }

    @Test
    fun theGraphAndItsLabelsAreReadBack() {
        val module = LibraryTesting()
        module.run(
            "store::saveArgument(t, [a], $a)",
            "store::saveArgument(t, [b], $b)",
            "store::saveSupport($a, $b)",
            "store::saveAttack(rebut, $b, $a, $a)",
            "store::labelArgument(in, $b)",
            "store::labelArgument(out, $a)",
            "store::labelStatement(in, [b])",
        )
        assertEquals("[($a, $b)]", module.value("store::graph(_, _, S)", "S"))
        // the attack on a extends to b, built on it
        assertEquals("[(rebut, $b, $b, $a), (rebut, $b, $a, $a)]", module.value("store::graph(_, A, _)", "A"))
        assertEquals("[$b]", module.value("store::argumentLabelling(I, _, _)", "I"))
        assertEquals("[[b]]", module.value("store::statementLabelling(I, _, _)", "I"))
        assertTrue(module.holds("context_active(B), store::argumentLabelIn(B, out, $a)"))
    }
}
