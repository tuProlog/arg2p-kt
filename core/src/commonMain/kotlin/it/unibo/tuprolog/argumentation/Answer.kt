@file:Suppress("NON_EXPORTABLE_TYPE")

package it.unibo.tuprolog.argumentation

import it.unibo.tuprolog.argumentation.core.model.terms.TermValues
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.Var
import kotlin.js.JsExport
import kotlin.js.JsName
import it.unibo.tuprolog.solve.Solution as LogicSolution

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

/**
 * The answer of a query as the solvers of arg2p give it: the goal answerQuery(Query, In, Out, Und), and the [Answer]
 * its first yes solution gives
 */
@InternalArg2pApi
class AnswerQuery(
    query: Term,
) {
    private val labels = listOf(Var.of("In"), Var.of("Out"), Var.of("Und"))

    /** The goal answering the query */
    val goal: Struct = Struct.of("answerQuery", listOf(query) + labels)

    /** The answer [solution] gives, the first yes solution of [goal] (none for no solution) */
    fun answer(solution: LogicSolution?): Answer {
        if (solution == null) return Answer(emptyList(), emptyList(), emptyList())
        val (statementsIn, statementsOut, statementsUnd) =
            labels.map { label ->
                solution.substitution[label]!!
                    .castToList()
                    .toList()
                    .map { TermValues.of(it) }
            }
        return Answer(statementsIn, statementsOut, statementsUnd)
    }
}
