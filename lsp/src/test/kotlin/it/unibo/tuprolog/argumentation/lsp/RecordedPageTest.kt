package it.unibo.tuprolog.argumentation.lsp

import com.google.gson.JsonObject
import com.google.gson.JsonParser
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertTrue
import kotlin.time.Duration.Companion.seconds

/** The recorded page of the solver, as Python makes it */
class RecordedPageTest {
    private val tweety = "f1 :=> bird(tweety).\nr1 : bird(X) => flies(X).\nf2 :=> -flies(tweety).\n"
    private val grounded = "argumentLabellingMode(grounded).\n"

    // The recording embedded in [page], as JSON
    private fun recordingOf(page: String): JsonObject {
        val script = """<script type="application/json" id="recording">"""
        return JsonParser.parseString(page.substringAfter(script).substringBefore("</script>")).asJsonObject
    }

    private fun JsonObject.solutions() =
        getAsJsonArray("next")
            .flatMap { it.asJsonArray }
            .map { it.asJsonObject }
            .filter { it.get("type").asString == "solution" }
            .map { it.getAsJsonObject("solution") }

    @Test
    fun thePageEmbedsTheSolutionsWithTheirGraphsAndTheSettings() {
        val page = RecordedPage.page(tweety, "buildLabelSets(In, Out, Und)", grounded)
        val recording = recordingOf(page)
        assertEquals(tweety, recording.get("theory").asString)
        assertEquals(grounded, recording.get("flags").asString)
        assertEquals(
            "started",
            recording
                .getAsJsonArray("solve")
                .last()
                .asJsonObject
                .get("type")
                .asString,
        )
        val solution = recording.solutions().first()
        assertEquals("yes", solution.get("result").asString)
        val labels =
            solution.getAsJsonObject("graph").getAsJsonArray("arguments").associate {
                it.asJsonObject.get("conclusion").asString to it.asJsonObject.get("label").asString
            }
        assertEquals("in", labels.getValue("-flies(tweety)"))
        assertEquals("out", labels.getValue("flies(tweety)"))
        val settings = recording.getAsJsonObject("settings")
        assertEquals("grounded", settings.getAsJsonArray("argumentLabellingMode")[0].asString)
        assertFalse(recording.get("more").asBoolean)
        assertFalse(recording.get("timedOut").asBoolean)
        assertTrue(page.contains("<title>Arg2P · buildLabelSets(In, Out, Und)</title>"))
        assertTrue(page.contains(Playground.link(tweety, "buildLabelSets(In, Out, Und)", grounded)))
    }

    @Test
    fun thePageLoadsTheSolverFromJsDelivrInTheVersionOfTheComponents() {
        val page = RecordedPage.page(tweety)
        assertTrue(page.contains("https://cdn.jsdelivr.net/npm/@tuprolog/arg2p-ui@${Pages.UI_VERSION}/solver/+esm"))
        assertTrue(page.contains(Pages.style))
        assertFalse(page.contains("{{"))
        assertEquals(RecordedPage.DEFAULT_FLAGS, recordingOf(page).get("flags").asString)
    }

    @Test
    fun aTheoryThatDoesNotParseIsRecordedAsAFailure() {
        val recording = recordingOf(RecordedPage.page("f1 :=> bird(tweety"))
        val failed = recording.getAsJsonArray("solve").last().asJsonObject
        assertEquals("failed", failed.get("type").asString)
        assertEquals(
            "theory",
            failed
                .getAsJsonObject("error")
                .getAsJsonObject("syntax")
                .get("where")
                .asString,
        )
        assertTrue(recording.getAsJsonArray("next").isEmpty)
    }

    @Test
    fun theSolutionsPastTheMostAskedForAreLeftOutAndTold() {
        val theory = "r1 :=> a.\nr2 :=> -a.\n"
        val page = RecordedPage.page(theory, "buildLabelSets(In, Out, Und)", "argumentLabellingMode(complete).", maxSolutions = 2)
        val recording = recordingOf(page)
        assertEquals(2, recording.solutions().size)
        assertTrue(recording.get("more").asBoolean)
        assertFalse(recording.get("timedOut").asBoolean)
    }

    @Test
    fun aRunOutOfTimeIsStoppedAndTold() {
        val recording = RecordedPage.record("loop :- loop.\n", "loop", "", 5, 1.seconds)
        assertTrue(recording.timedOut)
        assertTrue(recording.more)
        assertTrue(recording.next.isEmpty())
    }

    @Test
    fun theRecordingCannotCloseItsScript() {
        val theory = "% </script><!-- & -->\nf1 :=> a.\n"
        val page = RecordedPage.page(theory, "a")
        val embedded = page.substringAfter("""<script type="application/json" id="recording">""").substringBefore("</script>")
        assertFalse(embedded.contains("<"))
        assertFalse(embedded.contains(">"))
        assertEquals(theory, recordingOf(page).get("theory").asString)
        assertTrue(page.contains("<title>Arg2P · a</title>"))
    }

    @Test
    fun theTitleIsEscaped() {
        val page = RecordedPage.page("f1 :=> a.\n", "X = '<b>'")
        assertTrue(page.contains("<title>Arg2P · X = &#39;&lt;b&gt;&#39;</title>"))
    }
}
