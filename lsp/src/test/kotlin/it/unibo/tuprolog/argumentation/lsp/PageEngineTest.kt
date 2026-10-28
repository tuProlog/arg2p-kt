package it.unibo.tuprolog.argumentation.lsp

import com.google.gson.JsonObject
import com.google.gson.JsonParser
import java.util.concurrent.CompletableFuture
import java.util.concurrent.TimeUnit
import kotlin.test.AfterTest
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFailsWith
import kotlin.test.assertTrue
import kotlin.time.Duration.Companion.milliseconds

/** The engine of the pages, as the pages speak to it */
class PageEngineTest {
    private val tweety = """f1 :=> bird(tweety).\nr1 : bird(X) => flies(X).\nf2 :=> -flies(tweety).\n"""
    private val flags = """argumentLabellingMode(grounded).\nstatementLabellingMode(statement).\nqueryMode.\n"""
    private val solving = Solving("test")
    private val engine = PageEngine(solving)

    @AfterTest
    fun close() {
        engine.close()
        solving.close()
    }

    // The request of [session], as JSON, sent
    private fun send(
        request: String,
        session: String = "page",
        engine: PageEngine = this.engine,
    ): CompletableFuture<List<Answer>> = engine.handle(session, JsonParser.parseString(request).asJsonObject)

    // The answers to [request] of [session], as JSON
    private fun ask(
        request: String,
        session: String = "page",
        engine: PageEngine = this.engine,
    ): List<JsonObject> = answered(send(request, session, engine))

    private fun answered(answers: CompletableFuture<List<Answer>>) =
        json.toJsonTree(answers.get(60, TimeUnit.SECONDS)).asJsonArray.map {
            it.asJsonObject
        }

    private fun solve(
        theory: String,
        query: String,
        flags: String = this.flags,
        id: Int = 1,
        session: String = "page",
    ) = ask("""{"type":"solve","id":$id,"theory":"$theory","query":"$query","flags":"$flags"}""", session)

    private fun List<JsonObject>.of(type: String) = single { it.get("type").asString == type }

    private fun next(
        id: Int = 1,
        session: String = "page",
    ) = ask("""{"type":"next","id":$id}""", session)

    @Test
    fun aQueryGivesItsSolutionsOneAtATimeWithTheirGraphs() {
        val started = solve("r1 :=> a.\\nr2 :=> -a.\\n", "buildLabelSets(In, Out, Und).", "argumentLabellingMode(complete).").of("started")
        assertEquals("buildLabelSets(In, Out, Und)", started.get("query").asString)
        val labellings = mutableListOf<String>()
        do {
            val answer = next().of("solution")
            val solution = answer.getAsJsonObject("solution")
            if (solution.get("result").asString == "yes") {
                labellings +=
                    solution.getAsJsonObject("graph").getAsJsonArray("arguments").joinToString { it.asJsonObject.get("label").asString }
            }
        } while (answer.get("hasNext").asBoolean)
        assertEquals(3, labellings.size)
        assertEquals(3, labellings.toSet().size)
    }

    @Test
    fun aGraphTellsTheStructureOfItsArgumentsAndItsProfile() {
        solve(tweety, "buildLabelSets")
        val graph = next().of("solution").getAsJsonObject("solution").getAsJsonObject("graph")
        assertEquals("aspic_plus", graph.get("profile").asString)
        val arguments = graph.getAsJsonArray("arguments").map { it.asJsonObject }
        val flies = arguments.single { it.get("conclusion").asString == "flies(tweety)" }
        assertEquals("r1", flies.get("topRule").asString)
        val supports = flies.getAsJsonArray("supports").map { id -> arguments.single { it.get("id") == id }.get("conclusion").asString }
        assertEquals(listOf("bird(tweety)"), supports)
        assertTrue(
            graph.getAsJsonArray("attacks").map { it.asJsonObject }.any {
                it.get("to") == flies.get("id") && it.get("type").asString == "rebut"
            },
        )
    }

    @Test
    fun theStagesOfAQueryCanBeReadBack() {
        solve(tweety, "arg2p::solve")
        val solution = next().of("solution").getAsJsonObject("solution")
        val branch = solution.get("branch").asInt
        assertTrue(branch > 0)
        val graph = ask("""{"type":"graphAt","id":1,"branch":$branch,"reqId":7}""").of("graph")
        assertEquals(7, graph.get("reqId").asInt)
        assertEquals(solution.getAsJsonObject("graph"), graph.getAsJsonObject("graph"))
        assertTrue(
            ask("""{"type":"graphAt","id":1,"branch":0,"reqId":8}""")
                .of("graph")
                .getAsJsonObject("graph")
                .getAsJsonArray("arguments")
                .isEmpty,
        )
    }

    @Test
    fun whatAQueryPrintsIsToldWithItsId() {
        solve("", "write(hello)", id = 4)
        val printed = next(4).of("output")
        assertEquals(4, printed.get("id").asInt)
        assertTrue("hello" in printed.get("text").asString)
    }

    @Test
    fun aTextThatDoesNotParseTellsWhereAndWhichItIs() {
        val theory = solve("a :=> b.\\nc :=> d(.", "q").of("failed").getAsJsonObject("error")
        assertEquals("Theory error", theory.get("name").asString)
        assertEquals(
            json.toJsonTree(ProblemData(2, 9, 1, "Unexpected token '.'; expected term", "theory")),
            theory.get("syntax"),
        )

        fun where(failed: List<JsonObject>) =
            failed
                .of("failed")
                .getAsJsonObject("error")
                .getAsJsonObject("syntax")
                .get("where")
                .asString
        assertEquals("flags", where(solve("a.", "a", "queryMode(")))
        assertEquals("query", where(solve("a.", "flies(")))
        assertEquals(
            "Query error",
            solve("a.", " ")
                .of("failed")
                .getAsJsonObject("error")
                .get("name")
                .asString,
        )
    }

    @Test
    fun textsAreCheckedWithoutSolving() {
        fun problem(
            text: String,
            kind: String,
        ) = ask("""{"type":"check","reqId":1,"text":"$text","kind":"$kind"}""").of("checked").get("problem")
        assertTrue(problem(tweety, "theory").isJsonNull)
        assertEquals(2, problem("a :=> b.\\nc :=> d(.", "theory").asJsonObject.get("line").asInt)
        assertTrue(problem("", "query").isJsonNull)
        assertEquals(1, problem("flies(", "query").asJsonObject.get("line").asInt)
    }

    @Test
    fun theSettingsInEffectComeFromTheTheoryAndTheFlags() {
        fun settings(
            theory: String,
            flags: String,
        ) = ask("""{"type":"settings","reqId":1,"theory":"$theory","flags":"$flags"}""").of("settings").getAsJsonObject("settings")
        val settings = settings("graphBuildMode(abstract).\\na -> b.", flags)
        assertEquals("abstract", settings.getAsJsonArray("graphBuildMode")[0].asString)
        assertTrue(settings.getAsJsonArray("queryMode").isEmpty)
        assertEquals("grounded", settings.getAsJsonArray("argumentLabellingMode")[0].asString)
        // a theory that does not parse: the flags alone; flags that do not: nothing
        assertEquals("aspic_plus", settings("a :=> b", flags).getAsJsonArray("graphBuildMode")[0].asString)
        assertTrue(settings(tweety, "queryMode(").isEmpty)
    }

    @Test
    fun eachPageHasItsQueries() {
        solve(tweety, "flies(tweety)", session = "one")
        solve(tweety, "bird(tweety)", session = "two")
        assertEquals(
            "flies(tweety)",
            next(session = "one")
                .of("solution")
                .getAsJsonObject("solution")
                .get("query")
                .asString,
        )
        assertEquals(
            "bird(tweety)",
            next(session = "two")
                .of("solution")
                .getAsJsonObject("solution")
                .get("query")
                .asString,
        )
        assertEquals(
            PageEngine.NO_LONGER_RUNNING,
            next(session = "three")
                .of("failed")
                .getAsJsonObject("error")
                .get("message")
                .asString,
        )
    }

    @Test
    fun aPageKeepsItsLastQueryAlone() {
        solve(tweety, "flies(tweety)", id = 1)
        solve(tweety, "bird(tweety)", id = 2)
        assertEquals("failed", next(1).single().get("type").asString)
        assertEquals("solution", next(2).single().get("type").asString)
    }

    @Test
    fun aQueryThatDoesNotEndIsStopped() {
        solve("loop :- loop.", "loop")
        val endless = send("""{"type":"next","id":1}""")
        Thread.sleep(500)
        assertTrue(ask("""{"type":"stop"}""").isEmpty())
        val stopped = answered(endless).of("failed").getAsJsonObject("error")
        assertEquals(PageEngine.STOPPED, stopped.get("message").asString)
        assertIdle()
        // the page is still there: it solves its next query
        solve(tweety, "buildLabelSets", id = 2)
        assertEquals(
            "yes",
            next(2)
                .of("solution")
                .getAsJsonObject("solution")
                .get("result")
                .asString,
        )
    }

    @Test
    fun theRequestsOfAPageAreSolvedInTheirOrder() {
        val started = send("""{"type":"solve","id":1,"theory":"$tweety","query":"buildLabelSets","flags":""}""")
        val solution = send("""{"type":"next","id":1}""")
        assertEquals("started", answered(started).single().get("type").asString)
        assertEquals(
            "yes",
            answered(solution)
                .of("solution")
                .getAsJsonObject("solution")
                .get("result")
                .asString,
        )
    }

    @Test
    fun aNewQueryStopsTheOneUnderWay() {
        solve("loop :- loop.", "loop")
        val endless = send("""{"type":"next","id":1}""")
        Thread.sleep(500)
        solve(tweety, "bird(tweety)", id = 2)
        assertEquals("failed", answered(endless).single().get("type").asString)
        assertEquals("solution", next(2).single().get("type").asString)
    }

    @Test
    fun aRequestGivenUpByItsServerStops() {
        solve("loop :- loop.", "loop")
        val endless = send("""{"type":"next","id":1}""")
        Thread.sleep(500)
        endless.cancel(true)
        assertIdle()
    }

    @Test
    fun aClosedPageIsForgottenAndStopped() {
        solve("loop :- loop.", "loop")
        val endless = send("""{"type":"next","id":1}""")
        Thread.sleep(500)
        assertTrue(ask("""{"type":"close"}""").isEmpty())
        assertEquals("failed", answered(endless).single().get("type").asString)
        assertIdle()
        assertEquals(0, engine.pages)
    }

    @Test
    fun theIdlePagesAreForgotten() {
        val forgetful = PageEngine(solving, 200.milliseconds)
        try {
            ask("""{"type":"solve","id":1,"theory":"a.","query":"a","flags":""}""", engine = forgetful)
            Thread.sleep(600)
            assertEquals(0, forgetful.pages)
            assertEquals("failed", ask("""{"type":"next","id":1}""", engine = forgetful).single().get("type").asString)
        } finally {
            forgetful.close()
        }
    }

    @Test
    fun theLeastRecentlyUsedPagesAreForgotten() {
        solve(tweety, "bird(tweety)", session = "first")
        repeat(PageEngine.MAX_SESSIONS) { ask("""{"type":"warmUp"}""", "page $it").of("warm") }
        assertEquals(PageEngine.MAX_SESSIONS, engine.pages)
        assertEquals("failed", next(session = "first").single().get("type").asString)
    }

    @Test
    fun aRequestThatIsNotOneOfTheEngineIsInvalid() {
        assertFailsWith<InvalidParams> { send("""{"type":"jump"}""") }
        assertFailsWith<InvalidParams> { send("""{"id":1}""") }
        assertFailsWith<InvalidParams> { send("""{"type":"next","id":"one"}""") }
        assertFailsWith<InvalidParams> { send("""{"type":"graphAt","id":1,"reqId":1}""") }
    }

    // No thread is left solving
    private fun assertIdle() {
        val until = System.nanoTime() + TimeUnit.SECONDS.toNanos(10)
        while (solving.busy > 0 && System.nanoTime() < until) Thread.sleep(50)
        assertEquals(0, solving.busy)
    }
}
