package it.unibo.tuprolog.argumentation.lsp

import com.google.gson.Gson
import com.google.gson.JsonParser
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertTrue

/** The links to the web playground, against a link the playground made itself */
class PlaygroundTest {
    private val flags = "argumentLabellingMode(grounded).\nstatementLabellingMode(statement).\nqueryMode.\n"
    private val theory =
        "% the penguin, café\nf1 :=> bird(tweety).\nr1 : bird(X) => flies(X).\nf2 :=> -flies(tweety).\nsup(f2, r1).\n" +
            "name('Hello \"world\"').\n"
    private val query = "buildLabelSets(In, Out, Und)"

    // The state above, as the playground writes it
    private val state = Gson().toJsonTree(listOf(2, flags, theory, query))

    @Test
    fun aLinkOfThePlaygroundIsReadWithItsDictionary() {
        assertEquals(state, JsonParser.parseString(Playground.state(PLAYGROUND)))
    }

    @Test
    fun aLinkOpensItsTheoryInThePlayground() {
        val link = Playground.link(theory, query, flags)
        assertTrue(link.startsWith(Playground.URL + "?s="))
        // deflate writes the same state in more ways than one: what the playground reads is the state
        assertEquals(state, JsonParser.parseString(Playground.state(link.substringAfter("?s="))))
    }

    private companion object {
        // The state above, packed by the playground: shareUrl of links.ts in arg2p-kt-web, with its dictionary and fflate.
        // The links of the server it reads back as well (inflateSync with the same dictionary).
        const val PLAYGROUND =
            "AwUKYfeS6hglHSVVhZKMVIWC1Lz00kxgKCcnph1eGZOXZgh2U1JmUYpGSXlqakklSGsRUBQiFqEJcm5aTmZqMZANlEozAmvQhQghtBSXFmikGekoFBm" +
                "CeHmJuaka6h5Ap-UrxCiV5xflpMQoqYNkgC7BF-dKsQA"
    }
}
