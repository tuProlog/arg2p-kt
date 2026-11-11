package it.unibo.tuprolog.argumentation

import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFailsWith
import kotlin.test.assertFalse
import kotlin.test.assertTrue

/** The API of arg2p: solvers, their answers, graphs and solutions, values, flags and causality */
class Arg2pTest {
    private val conflict = "r1 : [] => a.\nr2 : [] => -a.\nsup(r1, r2)."

    @Test
    fun aQueryIsAnswered() {
        val answer = Arg2p.of(conflict).answer("a").single()
        assertEquals(listOf(Value.Atom("a")), answer.inStatements)
        assertEquals("in", answer.label("a"))
        assertEquals(Answer(emptyList(), listOf(Value.parse("-a")), emptyList()), Arg2p.of(conflict).answer("-a").single())
    }

    // Two preferred labellings, one with d IN and one with -d IN, c IN in both: an answer for each (enumerate), or one across
    // them, d and -d IN as each is in some (credulous), OUT as each is out in some (skeptical)
    @Test
    fun aQueryIsAnsweredForEachLabellingOrAcrossThem() {
        val theory = "f1 :=> d.\nf2 :=> -d.\nf3 :=> c."
        val flags = Flags.defaults().argumentLabellingMode(Semantics.PREFERRED)
        val labels = { answer: Answer ->
            listOf(answer.inStatements, answer.outStatements, answer.undStatements).map { it.map(Value::toString).toSet() }
        }
        val enumerated =
            Arg2p
                .of(theory, flags)
                .answer("X")
                .map(labels)
                .toSet()
        assertEquals(
            setOf(listOf(setOf("c", "d"), setOf("- d"), emptySet()), listOf(setOf("c", "- d"), setOf("d"), emptySet())),
            enumerated,
        )
        val credulous = Arg2p.of(theory, flags.answerMode(AnswerMode.CREDULOUS))
        assertEquals(listOf(setOf("c", "d", "- d"), emptySet(), emptySet()), labels(credulous.answer("X").single()))
        assertEquals(1, credulous.answer("X").size)
        val skeptical = Arg2p.of(theory, flags.answerMode(AnswerMode.SKEPTICAL))
        assertEquals(listOf(setOf("c"), setOf("d", "- d"), emptySet()), labels(skeptical.answer("X").single()))
        assertTrue(skeptical.solve("solve(c)").hasNext())
        assertFalse(skeptical.solve("solve(d)").next().isYes)
    }

    @Test
    fun theGraphOfEachLabelling() {
        val (graph) = Arg2p.of(conflict).graphs()
        assertEquals(listOf("A0 : r2 : - a", "A1 : r1 : a"), graph.arguments.map { it.descriptor })
        assertEquals(listOf("a"), graph.conclusions("in"))
        assertEquals(listOf("- a"), graph.conclusions("out"))
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
        assertTrue(arg2p.solve("solve").next().isYes)
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
        assertEquals(listOf("a", "f(- b)", "[1, 2.5]", "Y"), solutions.map { it.text("X") })
        assertEquals(listOf("atom", "struct", "list", "variable"), solutions.map { it.value("X")!!.kind })
        assertEquals(Value.List(listOf(Value.Integer(1), Value.Real(2.5))), solutions[2].bindings["X"])
        assertEquals(
            "no",
            Arg2p
                .of()
                .solve("member(x, [])")
                .next()
                .result,
        )
    }

    @Test
    fun anErrorHaltsTheSolutionAndIsThrownByAnAnswer() {
        // a rule concludes an assumption: the theory is not flat ABA
        val nonFlat = "assumption(a).\nassumption(b).\ncontrary(a, x).\ncontrary(b, y).\nr0 : b -> a."
        val error = assertFailsWith<Arg2pException> { Arg2p.of(nonFlat, Flags.defaults().graphBuildMode(Profile.ABA)).answer("a") }
        assertTrue("not flat" in error.message.orEmpty(), error.message)
        assertEquals(
            "halt",
            Arg2p
                .of()
                .solve("throw(error(type_error(integer, a), x))")
                .next()
                .result,
        )
        assertFailsWith<Arg2pException> { Arg2p.of().solve("f(") }
    }

    @Test
    fun theSettingsInEffectComeFromTheTheoryTheFlagsAndTheProfile() {
        val aspic = Arg2p.of("r1 : [] => a.")
        assertTrue("graphBuildMode" in aspic.knownFlags && "queryMode" in aspic.knownFlags)
        assertEquals(listOf("aspicPlus"), aspic.setting("graphBuildMode"))
        // the extensions the profile presets, the switches it turns on
        assertEquals(setOf("standardPref", "rebutRestriction"), aspic.setting("graphExtension").toSet())
        assertEquals(listOf("true"), aspic.setting("autoTransposition"))
        // the theory states its profile, which presets the others
        val abstract = Arg2p.of("graphBuildMode(abstract).\na -> b.\nargumentLabellingMode(preferred).")
        assertEquals(listOf("abstract"), abstract.setting("graphBuildMode"))
        assertEquals(listOf("preferred"), abstract.setting("argumentLabellingMode"))
        assertEquals(emptyList(), abstract.setting("graphExtension"))
        assertEquals(emptyList(), abstract.setting("queryMode"))
        assertEquals(listOf("https://example.org/t"), Arg2p.of("modulesPath('https://example.org/t').").setting("modulesPath"))
        assertFailsWith<Arg2pException> { aspic.setting("noSuchFlag") }
    }

    @Test
    fun theConflictModelInEffectIsToldByItsParts() {
        val all = listOf("negation", "deontic", "preference", "declared")
        assertEquals(all, Arg2p.of("r1 : [] => a.").setting("conflictModel"))
        assertEquals(all, Arg2p.of("conflictModel(builtin).").setting("conflictModel"))
        assertEquals(listOf("declared"), Arg2p.of("conflictModel(declared).").setting("conflictModel"))
        assertEquals(emptyList(), Arg2p.of("conflictModel(none).").setting("conflictModel"))
        // the flags of the solver: several parts, none, or all of them; the profile has its own (ABA) unless they state some
        val parts = Flags.none().conflictModel(ConflictModel.NEGATION, ConflictModel.PREFERENCE)
        assertEquals("conflictModel(negation).\nconflictModel(preference).\n", parts.toString())
        assertEquals(listOf("negation", "preference"), Arg2p.of("", parts).setting("conflictModel"))
        assertEquals(emptyList(), Arg2p.of("", Flags.none().conflictModel()).setting("conflictModel"))
        val aba = { flags: Flags -> Arg2p.of("graphBuildMode(aba).", flags).setting("conflictModel") }
        assertEquals(listOf("declared"), aba(Flags.defaults()))
        assertEquals(all, aba(Flags.defaults().conflictModel(ConflictModel.BUILTIN)))
        assertEquals(all, aba(Flags.defaults().conflictModel(*ConflictModel.entries.toTypedArray())))
        assertEquals(listOf("negation"), aba(Flags.defaults().conflictModel(ConflictModel.NEGATION)))
    }

    @Test
    fun textsThatDoNotParseTellWhere() {
        val failure = assertFailsWith<Arg2pException> { Arg2p.of("a :=> b.\nc :=> d(.") }
        assertEquals("Theory: Unexpected token '.'; expected term at 2:9", failure.message)
        val theory = failure.syntax
        assertEquals(2, theory?.line)
        assertEquals(9, theory?.column)
        assertEquals(".", theory?.token)
        assertEquals("Unexpected token '.'; expected term", theory?.description)
        assertEquals(1, assertFailsWith<Arg2pException> { Arg2p.of().solve("flies(") }.syntax?.line)
        assertEquals(1, assertFailsWith<Arg2pException> { Flags.parse("queryMode(") }.syntax?.line)
        assertEquals(7, assertFailsWith<Arg2pException> { Value.parse("flies(") }.syntax?.column)
        assertEquals(theory, Arg2p.checkTheory("a :=> b.\nc :=> d(."))
        // a theory is read with the notation of its languages: an abstract framework has no rules
        assertEquals(null, Arg2p.checkTheory("graphBuildMode(abstract).\na <-> b.\nX -> Y :- beats(X, Y)."))
        assertEquals(3, Arg2p.checkTheory("graphBuildMode(abstract).\na <-> b.\nr1 : bird(X) => flies(X).")?.line)
        assertEquals(null, Arg2p.checkQuery("solve(In, Out, Und)."))
        assertEquals(1, Arg2p.checkQuery("flies(")?.line)
    }

    @Test
    fun theTheoryChanges() {
        val arg2p = Arg2p.of("f1 :=> a.")
        arg2p.add("f2 :=> b.")
        assertEquals(listOf(Value.Atom("b")), arg2p.answer("b").single().inStatements)
        arg2p.load("f3 :=> c.")
        assertTrue(
            arg2p
                .answer("b")
                .single()
                .inStatements
                .isEmpty(),
        )
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
        assertEquals("- a", Value.parse("-a").toString())
        assertEquals("[- a, o(- b)]", Value.parse("[-a, o(-b)]").toString())
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
            "queryMode(off).\n",
            Flags
                .none()
                .queryMode(true)
                .queryMode(false)
                .toString(),
        )
        assertEquals(Flags.none().graphBuildMode(Profile.ABA_PLUS), Flags.parse("graphBuildMode(abaPlus)."))
        // the defaults state nothing: the theory, its profile and the defaults of the flags decide
        assertEquals("", Flags.defaults().toString())
    }

    @Test
    fun theFlagsOfTheSolverAreTheOnesItUses() {
        val theory = "f1 :=> d.\nf2 :=> -d."
        val grounded = Arg2p.of(theory).answer("d").single()
        assertEquals(listOf(Value.Atom("d")), grounded.undStatements)
        val stable = Arg2p.of(theory, Flags.defaults().argumentLabellingMode(Semantics.STABLE)).graphs()
        assertEquals(setOf(listOf("d"), listOf("- d")), stable.map { it.conclusions("in") }.toSet())
    }

    @Test
    fun theCausalityOfTheTheory() {
        val arg2p = Arg2p.of("r1 :=> a.\nr2 : a => b.")
        assertEquals(Value.parse("[-a]"), arg2p.solve("causality::ness(X, a, b)").next().value("X"))
        assertTrue(arg2p.solve("causality::butFor(a, b)").next().isYes)
        assertTrue(arg2p.solve("causality::nessIntervention([-a], b)").next().isYes)
        assertTrue(arg2p.solve("causality::ness(X, b, a)").next().isNo)
        assertTrue(arg2p.solve("causality::butFor(b, a)").next().isNo)
    }

    // A causality goal answers as on a fresh solver, whatever the solver did before: other solves, another theory, more
    // clauses (the store follows the version of the theory, no context reset asked)
    @Test
    fun theCausalityOfATheoryIsThatOfAFreshSolver() {
        val goals =
            listOf(
                "causality::ness(X, a, b)",
                "causality::nessOriginal(X, e, b)",
                "causality::nessIntervention([-a], b)",
                "causality::butFor(a, b)",
                "findall(Id-P, store::rule(Id, P, _), X)",
            )

        fun results(arg2p: Arg2p) = goals.map { goal -> arg2p.solve(goal).toList().map { "${it.result} ${it.text("X")}" } }
        val first = "f1 :=> a.\nf2 :=> c.\nr1 : a => b.\nr2 : c, e => b.\nf3 :=> e.\nr5 : m => n."
        val second = "f1 :=> a.\nr1 : a => b.\nr3 : g => -b.\nf4 :=> h.\nr4 : h, k => -a.\nr6 : p, q => z."
        val arg2p = Arg2p.of(first)
        assertEquals(results(Arg2p.of(first)), results(arg2p))
        arg2p.answer("b")
        arg2p.graphs()
        assertEquals(results(Arg2p.of(first)), results(arg2p))
        arg2p.load(second)
        assertEquals(results(Arg2p.of(second)), results(arg2p))
        arg2p.add("f9 :=> k.")
        assertEquals(results(Arg2p.of("$second\nf9 :=> k.")), results(arg2p))
    }

    @Test
    fun theVersionOfTheBuild() {
        // a release (0.17.0) or a development version (0.17.1-dev01-abcdef0)
        assertTrue(Regex("""\d+\.\d+\.\d+.*""").matches(Arg2p.version()), Arg2p.version())
    }
}
