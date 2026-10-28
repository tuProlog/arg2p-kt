@file:Suppress("NON_EXPORTABLE_TYPE")

package it.unibo.tuprolog.argumentation

import kotlin.js.JsExport
import kotlin.js.JsName

/** The statements for a query labelled in, out and und (see [Arg2p.answer]) */
@JsExport
data class Answer(
    @JsExport.Ignore @JsName("inStatementsList") val inStatements: List<Value>,
    @JsExport.Ignore @JsName("outStatementsList") val outStatements: List<Value>,
    @JsExport.Ignore @JsName("undStatementsList") val undStatements: List<Value>,
) {
    /** The statements labelled in (an array in JavaScript) */
    @JsName("inStatements")
    val inArray: Array<Value> get() = inStatements.toTypedArray()

    /** The statements labelled out (an array in JavaScript) */
    @JsName("outStatements")
    val outArray: Array<Value> get() = outStatements.toTypedArray()

    /** The statements labelled und (an array in JavaScript) */
    @JsName("undStatements")
    val undArray: Array<Value> get() = undStatements.toTypedArray()

    /** The label of [statement], Prolog text: in, out or und, null for a statement the query did not label */
    fun label(statement: String): String? =
        Value.parse(statement).let { value ->
            when (value) {
                in inStatements -> "in"
                in outStatements -> "out"
                in undStatements -> "und"
                else -> null
            }
        }
}
