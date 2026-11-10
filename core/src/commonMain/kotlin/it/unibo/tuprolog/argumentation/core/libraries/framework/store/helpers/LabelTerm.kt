package it.unibo.tuprolog.argumentation.core.libraries.framework.store.helpers

import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term

/** The term of the module `label` as Kotlin reads and builds it */
object LabelTerm {
    /** The labels: in, out and und */
    val labels: kotlin.collections.List<String> = listOf("in", "out", "und")

    /** The fact labelling [argument] with [label] */
    fun argument(
        label: String,
        argument: Term,
    ): Struct = Struct.of(label, argument)

    /** The functor of the facts labelling the arguments with [label] by their identifiers: inId, outId, undId */
    fun identifierFunctor(label: String): String =
        when (label) {
            "in" -> "inId"
            "out" -> "outId"
            "und" -> "undId"
            else -> "${label}Id"
        }
}
