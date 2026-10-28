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
    fun encode(message: KbMessage): String =
        TextWriter()
            .apply {
                when (message) {
                    Reset -> text("Reset")
                    Rebuild -> text("Rebuild")
                    is Load -> text("Load").text(message.theory).text(message.flags)
                    is Add -> text("Add").text(message.elem)
                    is RequireEvaluation -> text("RequireEvaluation").text(message.elem).text(message.replyTo)
                    is EvaluationResponse ->
                        text("EvaluationResponse").responses(message.inArgs).responses(message.outArgs).responses(message.undArgs)
                    is Subscribe -> text("Subscribe").number(message.worker).terms(message.premises).terms(message.conclusions)
                    is Build -> text("Build").interests(message.interests).text(message.locality ?: "")
                    is Explore -> text("Explore").number(message.from).terms(message.statements)
                    is Notify -> text("Notify").number(message.from).shared(message.arguments)
                    is Publish -> text("Publish").number(message.worker).shared(message.arguments)
                    is Ack -> text("Ack").number(message.from)
                    Attack -> text("Attack")
                    is Wanted -> text("Wanted").number(message.worker).terms(message.statements)
                    is Candidates -> text("Candidates").number(message.worker).shared(message.arguments)
                    is AttacksFound -> text("AttacksFound").number(message.worker).attacks(message.attacks)
                }
            }.toString()

    fun decode(text: String): KbMessage =
        TextReader(text).run {
            when (val kind = text()) {
                "Reset" -> Reset
                "Rebuild" -> Rebuild
                "Load" -> Load(text(), text())
                "Add" -> Add(text())
                "RequireEvaluation" -> RequireEvaluation(text(), text())
                "EvaluationResponse" -> EvaluationResponse(responses(), responses(), responses())
                "Subscribe" -> Subscribe(number(), terms(), terms())
                "Build" -> Build(interests(), text().ifEmpty { null })
                "Explore" -> Explore(number(), terms())
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

    private fun TextWriter.responses(values: List<Response>) = list(values) { text(it.claim).texts(it.queryChain) }

    private fun TextWriter.terms(values: List<Term>) = list(values) { text(it.toString()) }

    private fun TextWriter.shared(values: List<SharedArgument>) =
        list(values) { text(it.id).text(it.argument.toString()).texts(it.supports) }

    private fun TextWriter.interests(values: List<Interest>) = list(values) { number(it.worker).terms(it.premises).terms(it.conclusions) }

    private fun TextWriter.attacks(values: List<DirectAttack>) = list(values) { text(it.type).text(it.attacker).text(it.target) }

    private fun TextReader.responses() = List(number()) { Response(text(), texts()) }

    private fun TextReader.term() = Term.parse(text(), operators)

    private fun TextReader.terms() = List(number()) { term() }

    private fun TextReader.shared() = List(number()) { SharedArgument(text(), term(), texts()) }

    private fun TextReader.interests() = List(number()) { Interest(number(), terms(), terms()) }

    private fun TextReader.attacks() = List(number()) { DirectAttack(text(), text(), text()) }

    private val operators: OperatorSet by lazy { Arg2pSolver.default().operators() }
}

/** Fields written as text: each as its length, a colon and its characters (see [TextReader]) */
internal class TextWriter {
    private val builder = StringBuilder()

    fun text(value: String) = apply { builder.append(value.length).append(':').append(value) }

    fun number(value: Int) = text(value.toString())

    fun texts(values: List<String>) = list(values) { text(it) }

    fun <T> list(
        values: List<T>,
        write: TextWriter.(T) -> Unit,
    ) = apply {
        number(values.size)
        values.forEach { write(it) }
    }

    override fun toString() = builder.toString()
}

/** Fields read back from the text of a [TextWriter], in the order they were written */
internal class TextReader(
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
}
