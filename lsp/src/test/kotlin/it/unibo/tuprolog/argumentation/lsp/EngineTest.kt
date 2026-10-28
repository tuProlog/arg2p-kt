package it.unibo.tuprolog.argumentation.lsp

import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertNotNull
import kotlin.test.assertNull
import kotlin.test.assertTrue

class EngineTest {
    private val tweety = "f1 :=> bird(tweety).\nr1 : bird(X) => flies(X).\nf2 :=> -flies(tweety).\n"
    private val flags = "argumentLabellingMode(grounded).\nstatementLabellingMode(statement).\nqueryMode.\n"

    @Test
    fun aQueryGivesItsSolutionsOneAtATimeWithTheirGraphs() {
        val engine = Engine()
        val started = engine.solve("r1 :=> a.\nr2 :=> -a.\n", "buildLabelSets(In, Out, Und).", "argumentLabellingMode(complete).")
        assertNull(started.error)
        assertEquals("buildLabelSets(In, Out, Und)", started.query)
        val labellings = mutableListOf<String>()
        do {
            val answer = engine.next(started.id)
            val solution = assertNotNull(answer.solution)
            if (solution.result == "yes") labellings += solution.graph.arguments.joinToString { it.label }
        } while (answer.hasNext)
        assertEquals(3, labellings.size)
        assertEquals(3, labellings.toSet().size)
    }

    @Test
    fun aGraphTellsTheStructureOfItsArgumentsAndItsProfile() {
        val engine = Engine()
        val started = engine.solve(tweety, "buildLabelSets", flags)
        val graph = assertNotNull(engine.next(started.id).solution).graph
        assertEquals("aspic_plus", graph.profile)
        val flies = graph.arguments.single { it.conclusion == "flies(tweety)" }
        assertEquals("r1", flies.topRule)
        assertEquals(listOf("bird(tweety)"), flies.supports.map { id -> graph.arguments.single { it.id == id }.conclusion })
        assertTrue(graph.attacks.any { it.to == flies.id && it.type == "rebut" })
    }

    @Test
    fun theStagesOfARunCanBeReadBack() {
        val engine = Engine()
        val started = engine.solve(tweety, "arg2p::solve", flags)
        val solution = assertNotNull(engine.next(started.id).solution)
        assertTrue(solution.branch > 0)
        assertEquals(solution.graph, engine.graphAt(started.id, solution.branch))
        assertEquals(emptyList(), engine.graphAt(started.id, 0).arguments)
        engine.close(started.id)
        assertNotNull(engine.next(started.id).error)
    }

    @Test
    fun whatARunPrintsIsToldWithItsId() {
        val printed = mutableListOf<Pair<Int, String>>()
        val engine = Engine { id, text -> printed += id to text }
        val started = engine.solve("", "write(hello)", flags)
        engine.next(started.id)
        assertTrue(printed.any { it.first == started.id && "hello" in it.second }, printed.toString())
    }

    @Test
    fun aTextThatDoesNotParseTellsWhereAndWhichItIs() {
        val engine = Engine()
        val theory = assertNotNull(engine.solve("a :=> b.\nc :=> d(.", "q", flags).error)
        assertEquals("Theory error", theory.name)
        assertEquals(ProblemData(2, 9, 1, "Unexpected token '.'; expected term", "theory"), theory.syntax)
        assertEquals(
            "flags",
            engine
                .solve("a.", "a", "queryMode(")
                .error
                ?.syntax
                ?.where,
        )
        assertEquals(
            "query",
            engine
                .solve("a.", "flies(", flags)
                .error
                ?.syntax
                ?.where,
        )
        assertEquals("Query error", engine.solve("a.", " ", flags).error?.name)
    }

    @Test
    fun textsAreCheckedWithoutSolving() {
        assertNull(Engine.check(tweety, "theory"))
        assertEquals(2, Engine.check("a :=> b.\nc :=> d(.", "theory")?.line)
        assertNull(Engine.check("", "query"))
        assertEquals(1, Engine.check("flies(", "query")?.line)
    }

    @Test
    fun theSettingsInEffectComeFromTheTheoryAndTheFlags() {
        val settings = Engine.settings("graphBuildMode(abstract).\na -> b.", flags)
        assertEquals(listOf("abstract"), settings["graphBuildMode"])
        assertEquals(emptyList(), settings["queryMode"])
        assertEquals(listOf("grounded"), settings["argumentLabellingMode"])
        // a theory that does not parse: the flags alone; flags that do not: nothing
        assertEquals(listOf("aspic_plus"), Engine.settings("a :=> b", flags)["graphBuildMode"])
        assertEquals(emptyMap(), Engine.settings(tweety, "queryMode("))
    }
}
