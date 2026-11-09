package it.unibo.tuprolog.argumentation.parallel.message

import it.unibo.tuprolog.argumentation.Answer
import it.unibo.tuprolog.argumentation.Value
import it.unibo.tuprolog.argumentation.core.libraries.language.parser.helpers.TermValues
import it.unibo.tuprolog.argumentation.core.model.compactGraphOf
import it.unibo.tuprolog.argumentation.core.model.compactGraphText
import it.unibo.tuprolog.argumentation.engine.InternalArg2pApi
import it.unibo.tuprolog.argumentation.parallel.haltContext
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Substitution
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.solve.exception.HaltException
import it.unibo.tuprolog.solve.Solution as LogicSolution

/**
 * The messages of the clients and their answers as text, for a solver on another thread than its node (see
 * ThreadEnvironment), or always with the option textual of a node: a kind, then the fields, each written as its length, a
 * colon and its characters, so that no character needs escaping (as the frames between threads are written). The terms
 * are written as Prolog text, read again with the operators of arg2p; a solution of a goal as its goal, the values of
 * its variables by name, or why it halted.
 */
internal object MessageCodec {
    fun encode(message: KbMessage): String =
        TextWriter()
            .apply {
                when (message) {
                    Reset -> text("Reset")
                    Rebuild -> text("Rebuild")
                    Stop -> text("Stop")
                    is Configure -> text("Configure").text(message.flags)
                    is Load -> text("Load").text(message.theory)
                    is Add -> text("Add").text(message.elem)
                    is RequireEvaluation -> text("RequireEvaluation").text(message.elem).text(message.replyTo)
                    is RequireSolution -> text("RequireSolution").text(message.run).texts(listOfNotNull(message.goal)).text(message.replyTo)
                    is RequireAnswer -> text("RequireAnswer").text(message.query).text(message.replyTo)
                    is EvaluationResponse ->
                        text("EvaluationResponse")
                            .responses(message.inArgs)
                            .responses(message.outArgs)
                            .responses(message.undArgs)
                            .optional(message.error)
                    is AnswerResponse ->
                        text("AnswerResponse")
                            .text(if (message.answers == null) "0" else "1")
                            .list(message.answers.orEmpty()) { values(it.inStatements).values(it.outStatements).values(it.undStatements) }
                            .optional(message.error)
                    is SolutionResponse ->
                        text("SolutionResponse")
                            .text(message.run)
                            .solution(message.solution)
                            .text(compactGraphText(message.graph))
                            .text(message.output)
                            .optional(message.error)
                            .text(if (message.stopped) "1" else "0")
                }
            }.toString()

    fun decode(text: String): KbMessage =
        TextReader(text).run {
            when (val kind = text()) {
                "Reset" -> Reset
                "Rebuild" -> Rebuild
                "Stop" -> Stop
                "Configure" -> Configure(text())
                "Load" -> Load(text())
                "Add" -> Add(text())
                "RequireEvaluation" -> RequireEvaluation(text(), text())
                "RequireSolution" -> RequireSolution(text(), texts().singleOrNull(), text())
                "RequireAnswer" -> RequireAnswer(text(), text())
                "EvaluationResponse" -> EvaluationResponse(responses(), responses(), responses(), optional())
                "AnswerResponse" -> {
                    val some = text() == "1"
                    val answers = list { Answer(values(), values(), values()) }
                    AnswerResponse(if (some) answers else null, optional())
                }
                "SolutionResponse" ->
                    SolutionResponse(text(), solution(), compactGraphOf(text()), text(), optional(), text() == "1")
                else -> error("Unknown message $kind")
            }
        }

    private fun TextWriter.optional(value: String?) = texts(listOfNotNull(value))

    private fun TextReader.optional(): String? = texts().singleOrNull()

    private fun TextWriter.responses(values: List<Response>) = list(values) { text(it.claim).texts(it.queryChain) }

    private fun TextReader.responses() = list { Response(text(), texts()) }

    private fun TextWriter.values(values: List<Value>) = texts(values.map { it.toString() })

    private fun TextReader.values() = texts().map { Value.parse(it) }

    // A solution: none, or its kind, its goal, the values of the variables of the goal (by name) or why it halted
    private fun TextWriter.solution(solution: LogicSolution?) =
        apply {
            when {
                solution == null -> text("none")
                solution.isYes ->
                    text("yes").text(solution.query.toString()).list(solution.substitution.entries.toList()) { (variable, value) ->
                        text(variable.name).text(value.toString())
                    }
                solution.isNo -> text("no").text(solution.query.toString())
                else -> text("halt").text(solution.query.toString()).text(solution.exception?.message ?: "")
            }
        }

    private fun TextReader.solution(): LogicSolution? {
        val kind = text()
        if (kind == "none") return null
        val query = Struct.parse(text(), TermValues.operators)
        return when (kind) {
            "yes" -> {
                val values = list { text() to Term.parse(text(), TermValues.operators) }.toMap()
                val variables = query.variables.distinct().filter { it.name in values }
                LogicSolution.yes(query, Substitution.unifier(variables.associateWith { values.getValue(it.name) }))
            }
            "no" -> LogicSolution.no(query)
            else -> LogicSolution.halt(query, HaltException(message = text(), context = haltContext))
        }
    }
}

/** Fields written as text: each as its length, a colon and its characters (see [TextReader]) */
@InternalArg2pApi
class TextWriter {
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

/**
 * Fields read back from the text of a [TextWriter], in the order they were written: an IllegalArgumentException for a
 * text that is not one (as one from another process may be), whose lists are no longer than the text left can hold
 */
@InternalArg2pApi
class TextReader(
    private val source: String,
) {
    private var position = 0

    fun text(): String {
        val colon = source.indexOf(':', position)
        val length = if (colon < 0) null else source.substring(position, colon).toIntOrNull()
        require(length != null && length >= 0 && length <= source.length - colon - 1) { "Malformed text at $position" }
        return source.substring(colon + 1, colon + 1 + length).also { position = colon + 1 + length }
    }

    fun number() = text().let { requireNotNull(it.toIntOrNull()) { "Not a number: $it" } }

    fun texts() = list { text() }

    // The elements of a list, read by [read]: each takes two characters at least ("0:"), hence a count of more than
    // the text left can hold is refused before any is read
    fun <T> list(read: TextReader.() -> T): List<T> {
        val count = number()
        require(count in 0..(source.length - position) / 2) { "A list of $count elements in ${source.length - position} characters" }
        return List(count) { read() }
    }
}
