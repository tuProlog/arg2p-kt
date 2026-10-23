package it.unibo.tuprolog.argumentation.core.model.terms

import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term

/** The term of the module LabelModule as Kotlin reads and builds it (see [LabelModule]) */
object LabelTerm {
    /** The labels: in, out and und */
    val labels: kotlin.collections.List<String> = listOf("in", "out", "und")

    /** The fact labelling [argument] with [label] */
    fun argument(
        label: String,
        argument: Term,
    ): Struct = Struct.of(label, argument)

    /** The fact labelling the argument with identifier [id] with [label] */
    fun identifier(
        label: String,
        id: Term,
    ): Struct = Struct.of("${label}Id", id)

    /** The fact labelling [statement] with [label] */
    fun statement(
        label: String,
        statement: Term,
    ): Struct = Struct.of("stat" + label.replaceFirstChar { it.uppercase() }, statement)
}
