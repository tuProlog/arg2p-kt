package it.unibo.tuprolog.argumentation.parallel.message

import it.unibo.tuprolog.argumentation.core.Arg2pSolver
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.operators.OperatorSet
import it.unibo.tuprolog.core.parsing.parse

/**
 * The messages as text, for the transports that cannot carry objects (the threads of JS, the network): a kind, then
 * the fields, each written as its length, a colon and its characters, so that no character needs escaping. The terms
 * are written as Prolog text, and parsed again with the operators of arg2p.
 */
object MessageCodec {
    private val operators: OperatorSet by lazy { Arg2pSolver.default().operators() }

    fun encode(message: KbMessage): String =
        Writer()
            .apply {
                when (message) {
                    Reset -> kind("Reset")
                    is Load -> kind("Load").text(message.theory)
                    is Add -> kind("Add").text(message.elem)
                    is RequireEvaluation -> kind("RequireEvaluation").text(message.elem).text(message.replyTo)
                    is EvaluationResponse ->
                        kind("EvaluationResponse").responses(message.inArgs).responses(message.outArgs).responses(message.undArgs)
                    is Subscribe -> kind("Subscribe").number(message.worker).terms(message.premises)
                    is Build -> kind("Build").interests(message.interests)
                    is Notify -> kind("Notify").number(message.from).shared(message.arguments)
                    is Publish -> kind("Publish").number(message.worker).shared(message.arguments)
                    is Ack -> kind("Ack").number(message.from)
                    Attack -> kind("Attack")
                    is Wanted -> kind("Wanted").number(message.worker).terms(message.statements)
                    is Candidates -> kind("Candidates").number(message.worker).shared(message.arguments)
                    is AttacksFound -> kind("AttacksFound").number(message.worker).attacks(message.attacks)
                }
            }.toString()

    fun decode(text: String): KbMessage =
        Reader(text).run {
            when (val kind = text()) {
                "Reset" -> Reset
                "Load" -> Load(text())
                "Add" -> Add(text())
                "RequireEvaluation" -> RequireEvaluation(text(), text())
                "EvaluationResponse" -> EvaluationResponse(responses(), responses(), responses())
                "Subscribe" -> Subscribe(number(), terms())
                "Build" -> Build(interests())
                "Notify" -> Notify(number(), shared())
                "Publish" -> Publish(number(), shared())
                "Ack" -> Ack(number())
                "Attack" -> Attack
                "Wanted" -> Wanted(number(), terms())
                "Candidates" -> Candidates(number(), shared())
                "AttacksFound" -> AttacksFound(number(), attacks())
                else -> error("Unknown message $kind")
            }
        }

    private class Writer {
        private val builder = StringBuilder()

        fun kind(name: String) = text(name)

        fun text(value: String) = apply { builder.append(value.length).append(':').append(value) }

        fun number(value: Int) = text(value.toString())

        fun texts(values: List<String>) = list(values) { text(it) }

        fun responses(values: List<Response>) = list(values) { text(it.claim).texts(it.queryChain) }

        fun terms(values: List<Term>) = list(values) { text(it.toString()) }

        fun shared(values: List<SharedArgument>) = list(values) { text(it.id).text(it.argument.toString()).texts(it.supports) }

        fun interests(values: List<Interest>) = list(values) { number(it.worker).terms(it.premises) }

        fun attacks(values: List<DirectAttack>) = list(values) { text(it.type).text(it.attacker).text(it.target) }

        private fun <T> list(
            values: List<T>,
            write: (T) -> Unit,
        ) = apply {
            number(values.size)
            values.forEach(write)
        }

        override fun toString() = builder.toString()
    }

    private class Reader(
        private val source: String,
    ) {
        private var position = 0

        fun text(): String {
            val colon = source.indexOf(':', position)
            val length = source.substring(position, colon).toInt()
            return source.substring(colon + 1, colon + 1 + length).also { position = colon + 1 + length }
        }

        fun number() = text().toInt()

        fun texts() = List(number()) { text() }

        fun responses() = List(number()) { Response(text(), texts()) }

        fun term() = Term.parse(text(), operators)

        fun terms() = List(number()) { term() }

        fun shared() = List(number()) { SharedArgument(text(), term(), texts()) }

        fun interests() = List(number()) { Interest(number(), terms()) }

        fun attacks() = List(number()) { DirectAttack(text(), text(), text()) }
    }
}
