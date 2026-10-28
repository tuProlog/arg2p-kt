package it.unibo.tuprolog.argumentation

import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFailsWith
import kotlin.test.assertFalse
import kotlin.test.assertNull
import kotlin.test.assertTrue

/** The API of arg2p: solvers, their answers, graphs and solutions, values, flags and causality */
class Arg2pTest {
    private val conflict = "r1 : [] => a.\nr2 : [] => -a.\nsup(r1, r2)."

    @Test
    fun aQueryIsAnswered() {
        val answer = Arg2p.of(conflict).answer("a")
        assertEquals(listOf(Value.Atom("a")), answer.inStatements)
        assertEquals("in", answer.label("a"))
        assertEquals(Answer(emptyList(), listOf(Value.parse("-a")), emptyList()), Arg2p.of(conflict).answer("-a"))
    }

    @Test
    fun theGraphOfEachLabelling() {
        val (graph) = Arg2p.of(conflict).graphs()
        assertEquals(listOf("A0 : r2 : -a", "A1 : r1 : a"), graph.arguments.map { it.descriptor })
        assertEquals(listOf("a"), graph.conclusions("in"))
        assertEquals(listOf("-a"), graph.conclusions("out"))
        val attack = graph.attacks.single()
        assertEquals(listOf("A1", "A0", "rebut"), listOf(attack.attacker.identifier, attack.target.identifier, attack.type))
        assertEquals("in", graph.label(attack.attacker))
    }

    @Test
    fun aSemanticsWithSeveralLabellingsGivesAGraphForEach() {
        val flags = Flags.defaults().argumentLabellingMode(Semantics.PREFERRED)
        assertEquals(2, Arg2p.of("f1 :=> d.\nf2 :=> -d.", flags).graphs().size)
    }

    @Test
    fun theGraphOfTheLastResolution() {
        val arg2p = Arg2p.of(conflict)
        assertTrue(arg2p.solve("buildLabelSets").next().isYes)
        assertEquals(arg2p.graphAt(arg2p.activeBranch), arg2p.graph())
        assertEquals(2, arg2p.graph().arguments.size)
        assertTrue(arg2p.contextFacts(arg2p.activeBranch, "statIn(S)").next().isYes)
    }

    @Test
    fun theSolutionsOfAGoalBindItsVariables() {
        // the last solution is no: there are no others
        val solutions =
            Arg2p
                .of()
                .solve("member(X, [a, f(-b), [1, 2.5], Y])")
                .toList()
                .filter { it.isYes }
        assertEquals(listOf("a", "f(-b)", "[1, 2.5]", "Y"), solutions.map { it.text("X") })
        assertEquals(listOf("atom", "struct", "list", "variable"), solutions.map { it.value("X")!!.kind })
        assertEquals(Value.List(listOf(Value.Integer(1), Value.Real(2.5))), solutions[2].bindings["X"])
        assertFalse(
            Arg2p
                .of()
                .solve("member(x, [])")
                .next()
                .isYes,
        )
    }

    @Test
    fun anErrorHaltsTheSolutionAndIsThrownByAnAnswer() {
        // a rule concludes an assumption: the theory is not flat ABA
        val nonFlat = "assumption(a).\nassumption(b).\ncontrary(a, x).\ncontrary(b, y).\nr0 : b -> a."
        val error = assertFailsWith<Arg2pException> { Arg2p.of(nonFlat, Flags.defaults().graphBuildMode(Profile.ABA)).answer("a") }
        assertTrue("not flat" in error.message.orEmpty(), error.message)
        assertTrue(
            Arg2p
                .of()
                .solve("throw(error(type_error(integer, a), x))")
                .next()
                .isHalt,
        )
        assertFailsWith<Arg2pException> { Arg2p.of().solve("f(") }
    }

    @Test
    fun theTheoryChanges() {
        val arg2p = Arg2p.of("f1 :=> a.")
        arg2p.add("f2 :=> b.")
        assertEquals(listOf(Value.Atom("b")), arg2p.answer("b").inStatements)
        arg2p.load("f3 :=> c.")
        assertTrue(arg2p.answer("b").inStatements.isEmpty())
        assertTrue("f3" in arg2p.theory)
    }

    @Test
    fun whatTheSolverPrintsGoesToItsOutput() {
        val printed = StringBuilder()
        val arg2p = Arg2p.builder().output { printed.append(it) }.build()
        arg2p.solve("write(hello)").next()
        assertEquals("hello", printed.toString())
    }

    @Test
    fun valuesAreWrittenAsArg2pWritesThem() {
        assertEquals("-a", Value.parse("-a").toString())
        assertEquals("[-a, o(-b)]", Value.parse("[-a, o(-b)]").toString())
        assertEquals(Value.Struct("f", listOf(Value.Atom("a"), Value.Variable("X"))), Value.parse("f(a, X)"))
        assertEquals("'hello world'", Value.Atom("hello world").toString())
    }

    @Test
    fun flagsAreFacts() {
        assertEquals("argumentLabellingMode(preferred).\n", Flags.none().argumentLabellingMode(Semantics.PREFERRED).toString())
        assertEquals(
            "graphExtension(none).\nqueryMode.\n",
            Flags
                .none()
                .graphExtensions()
                .queryMode(true)
                .toString(),
        )
        assertEquals(
            "",
            Flags
                .none()
                .queryMode(true)
                .queryMode(false)
                .toString(),
        )
        assertEquals(Flags.none().graphBuildMode(Profile.ABA_PLUS), Flags.parse("graphBuildMode(aba_plus)."))
        assertTrue("graphBuildMode(aspic_plus)." in Flags.defaults().toString())
    }

    @Test
    fun theFlagsOfTheSolverAreTheOnesItUses() {
        val theory = "f1 :=> d.\nf2 :=> -d."
        val grounded = Arg2p.of(theory).answer("d")
        assertEquals(listOf(Value.Atom("d")), grounded.undStatements)
        val stable = Arg2p.of(theory, Flags.defaults().argumentLabellingMode(Semantics.STABLE)).graphs()
        assertEquals(setOf(listOf("d"), listOf("-d")), stable.map { it.conclusions("in") }.toSet())
    }

    @Test
    fun theCausalityOfTheTheory() {
        val causality = Arg2p.of("r1 :=> a.\nr2 : a => b.").causality
        assertEquals(Value.parse("[-a]"), causality.ness("a", "b"))
        assertTrue(causality.butFor("a", "b"))
        assertTrue(causality.nessIntervention("[-a]", "b"))
        assertNull(causality.ness("b", "a"))
        assertFalse(causality.butFor("b", "a"))
    }

    @Test
    fun theVersionOfTheBuild() {
        // a release (0.17.0) or a development version (0.17.1-dev01-abcdef0)
        assertTrue(Regex("""\d+\.\d+\.\d+.*""").matches(Arg2p.version()), Arg2p.version())
    }
}
