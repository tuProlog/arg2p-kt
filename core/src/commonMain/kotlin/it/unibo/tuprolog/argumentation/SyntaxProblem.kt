package it.unibo.tuprolog.argumentation

import it.unibo.tuprolog.core.parsing.ParseException
import kotlin.js.JsExport
import kotlin.jvm.JvmStatic

/**
 * Where a text (a theory, a query, flags) does not parse: its [line] and [column] (from 1), the [token] found there
 * when there is one, and why ([description], as `Unexpected token '.'; expected term`)
 */
@JsExport
class SyntaxProblem(
    val line: Int,
    val column: Int,
    val token: String?,
    val description: String,
) {
    /** How many characters the problem spans from [column]: the token, or one */
    val length: Int get() = token?.length?.takeIf { it > 0 } ?: 1

    override fun toString(): String = "$description at $line:$column"

    override fun equals(other: Any?): Boolean =
        other is SyntaxProblem &&
            other.line == line &&
            other.column == column &&
            other.token == token &&
            other.description == description

    override fun hashCode(): Int = ((line * 31 + column) * 31 + token.hashCode()) * 31 + description.hashCode()

    @JsExport.Ignore
    companion object {
        private val POSITION = Regex(""" at \d+:\d+""")

        /** The problem [exception] reports */
        @InternalArg2pApi
        @JvmStatic
        fun of(exception: ParseException): SyntaxProblem =
            SyntaxProblem(
                line = maxOf(1, exception.line),
                column = maxOf(1, exception.column),
                token = exception.offendingSymbol?.takeIf { it.isNotEmpty() },
                description = (exception.message ?: "Syntax error").replace(POSITION, ""),
            )

        /**
         * [parse] done, or an [Arg2pException] telling where the text it reads does not parse ([what] it is): its
         * message is the problem, as `Theory: Unexpected token '.'; expected term at 2:9`
         */
        @InternalArg2pApi
        @JvmStatic
        fun <T> parsing(
            what: String,
            parse: () -> T,
        ): T =
            try {
                parse()
            } catch (exception: ParseException) {
                of(exception).let { throw Arg2pException("$what: $it", it) }
            }

        /** Where [parse] finds that the text it reads does not parse, or null if it does */
        @InternalArg2pApi
        @JvmStatic
        fun check(parse: () -> Unit): SyntaxProblem? =
            try {
                parse()
                null
            } catch (exception: ParseException) {
                of(exception)
            }
    }
}
