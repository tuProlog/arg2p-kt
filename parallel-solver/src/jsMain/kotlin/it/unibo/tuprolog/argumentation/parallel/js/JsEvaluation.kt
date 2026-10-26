package it.unibo.tuprolog.argumentation.parallel.js

import it.unibo.tuprolog.argumentation.parallel.message.EvaluationResponse
import it.unibo.tuprolog.argumentation.parallel.message.Response
import kotlin.js.JsExport

/** An argument for the statement evaluated (its claim), with the arguments its label comes from, as Prolog text */
@JsExport
class JsResponse(
    val claim: String,
    val queryChain: Array<String>,
)

/** The arguments for a statement, by label */
@JsExport
class JsEvaluation(
    val inArgs: Array<JsResponse>,
    val outArgs: Array<JsResponse>,
    val undArgs: Array<JsResponse>,
) {
    internal companion object {
        fun of(response: EvaluationResponse) = JsEvaluation(js(response.inArgs), js(response.outArgs), js(response.undArgs))

        private fun js(responses: List<Response>) = responses.map { JsResponse(it.claim, it.queryChain.toTypedArray()) }.toTypedArray()
    }
}
