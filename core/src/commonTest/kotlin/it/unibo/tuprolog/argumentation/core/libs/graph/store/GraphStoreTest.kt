package it.unibo.tuprolog.argumentation.core.libs.graph.store

import it.unibo.tuprolog.argumentation.core.primitives.ids

import it.unibo.tuprolog.argumentation.core.ModuleTesting
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertTrue

class GraphStoreTest {
    private val a = "[[r1], r1, [a], [], [[r1], [r1], []]]"
    private val b = "[[r2, r1], r2, [b], [a], [[r2], [r2, r1], []]]"
    private val c = "[[r3], r3, [c], [], [[r3], [r3], []]]"

    @Test
    fun anArgumentIsSavedWithItsLookups() {
        val module = ModuleTesting()
        module.run("graphStore::saveArgument(tag, [a], $a)")
        assertTrue(module.holds("context_check(argument($a))"))
        assertTrue(module.holds("graphStore::argumentFor([a], X), X == $a"))
        assertTrue(module.holds("context_id(argument($a), Id), graphStore::argumentById(Id, X), X == $a"))
        assertTrue(module.holds("context_id(argument($a), Id), context_check(argInfo(Id, _, [[r1], [r1], []]))"))
        assertTrue(module.holds("context_check(newConc(tag, [a]))"))
    }

    @Test
    fun anArgumentIsSavedOnce() {
        val module = ModuleTesting()
        module.run("graphStore::saveArgument(tag, [a], $a)")
        assertFalse(module.holds("graphStore::saveArgument(tag, [a], $a)"))
        assertEquals(1, module.solutions("context_check(argument(X))").count { it.isYes })
    }

    @Test
    fun aSupportIsSavedByArgumentsAndIdentifiers() {
        val module = ModuleTesting()
        module.run("graphStore::saveArgument(t, [a], $a)", "graphStore::saveArgument(t, [b], $b)", "graphStore::saveSupport($a, $b)")
        assertTrue(module.holds("context_check(support($a, $b))"))
        assertTrue(module.holds("context_id(argument($a), IdA), context_id(argument($b), IdB), context_check(supportId(IdA, IdB))"))
        assertFalse(module.holds("graphStore::saveSupport($a, $b)"))
    }

    @Test
    fun anAttackIsSavedAndRemovedInAllItsForms() {
        val module = ModuleTesting()
        module.run(
            "graphStore::saveArgument(t, [a], $a)",
            "graphStore::saveArgument(t, [b], $b)",
            "graphStore::saveArgument(t, [c], $c)",
            "graphStore::saveAttack(rebut, $c, $b, $a)",
        )
        val ids = "context_id(argument($c), IdC), context_id(argument($b), IdB), context_id(argument($a), IdA)"
        assertTrue(module.holds("context_check(attack(rebut, $c, $b, $a))"))
        assertTrue(module.holds("$ids, context_check(attackId(rebut, IdC, IdB, IdA))"))
        assertTrue(module.holds("$ids, context_check(att(IdC, IdB))"))
        assertFalse(module.holds("graphStore::saveAttack(rebut, $c, $b, $a)"))

        module.run("graphStore::removeAttack(rebut, $c, $b, $a)")
        assertFalse(module.holds("context_check(attack(_, _, _, _))"))
        assertFalse(module.holds("context_check(attackId(_, _, _, _))"))
        assertFalse(module.holds("context_check(clause(att(_, _), _))"))
    }

    @Test
    fun argumentsAreLabelledByThemselvesAndByIdentifiers() {
        val module = ModuleTesting()
        module.run("graphStore::saveArgument(t, [a], $a)", "graphStore::argumentId($a, Id), graphStore::labelArgument(und, $a, Id)")
        assertTrue(module.holds("graphStore::argumentLabel(und, $a)"))
        assertTrue(module.holds("graphStore::argumentId($a, Id), graphStore::argumentLabelById(und, Id)"))
        module.run("graphStore::unlabelArguments(und)")
        assertFalse(module.holds("graphStore::argumentLabel(_, _)"))
        assertFalse(module.holds("graphStore::argumentLabelById(_, _)"))
    }

    @Test
    fun theGraphAndItsLabelsAreReadBack() {
        val module = ModuleTesting()
        module.run(
            "graphStore::saveArgument(t, [a], $a)",
            "graphStore::saveArgument(t, [b], $b)",
            "graphStore::saveSupport($a, $b)",
            "graphStore::saveAttack(rebut, $b, $a, $a)",
            "graphStore::labelArgument(in, $b)",
            "graphStore::labelArgument(out, $a)",
            "graphStore::labelStatement(in, [b])",
        )
        assertEquals("[($a, $b)]", module.value("graphStore::graph(_, _, S)", "S"))
        assertEquals("[(rebut, $b, $a, $a)]", module.value("graphStore::graph(_, A, _)", "A"))
        assertEquals("[$b]", module.value("graphStore::argumentLabelling(I, _, _)", "I"))
        assertEquals("[[b]]", module.value("graphStore::statementLabelling(I, _, _)", "I"))
        assertEquals("[b]", module.value("graphStore::claimLabelling(I, _, _)", "I"))
        assertTrue(module.holds("context_active(B), graphStore::argumentLabelIn(B, out, $a)"))
    }
}
