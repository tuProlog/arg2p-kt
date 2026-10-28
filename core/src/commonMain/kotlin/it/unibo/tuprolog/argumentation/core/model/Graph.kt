@file:Suppress("NON_EXPORTABLE_TYPE")

package it.unibo.tuprolog.argumentation.core.model

import kotlin.js.JsExport
import kotlin.js.JsName

/**
 * An argumentation graph: its [arguments], the [attacks] and the [supports] between them, and the label of each
 * argument ([labellings]), in the labelling the graph was read in
 */
@JsExport
data class Graph(
    @JsExport.Ignore @JsName("argumentsList") val arguments: List<Argument>,
    @JsExport.Ignore @JsName("attacksList") val attacks: List<Attack>,
    @JsExport.Ignore @JsName("supportsList") val supports: List<Support>,
) {
    @JsExport.Ignore
    @JsName("labellingsList")
    var labellings: List<LabelledArgument> = emptyList()

    /** The arguments (an array in JavaScript) */
    @JsName("arguments")
    val argumentArray: Array<Argument> get() = arguments.toTypedArray()

    /** The attacks (an array in JavaScript) */
    @JsName("attacks")
    val attackArray: Array<Attack> get() = attacks.toTypedArray()

    /** The supports (an array in JavaScript) */
    @JsName("supports")
    val supportArray: Array<Support> get() = supports.toTypedArray()

    /** The arguments with their labels (an array in JavaScript) */
    @JsName("labellings")
    val labellingArray: Array<LabelledArgument> get() = labellings.toTypedArray()

    /** The label of [argument]: in, out, und, or na for an argument the labelling does not label */
    fun label(argument: Argument): Label? = labellings.firstOrNull { it.argument == argument }?.label

    /** The arguments labelled [label] (in, out, und) */
    @JsExport.Ignore
    @JsName("labelledList")
    fun labelled(label: Label): List<Argument> = labellings.filter { it.label == label }.map { it.argument }

    /** The arguments labelled [label] (an array in JavaScript) */
    @JsName("labelled")
    fun labelledArray(label: Label): Array<Argument> = labelled(label).toTypedArray()

    /** The conclusions of the arguments labelled [label] (in, out, und), each once */
    @JsExport.Ignore
    @JsName("conclusionsList")
    fun conclusions(label: Label): List<Literal> = labelled(label).map { it.conclusion }.distinct()

    /** The conclusions of the arguments labelled [label] (an array in JavaScript) */
    @JsName("conclusions")
    fun conclusionArray(label: Label): Array<Literal> = conclusions(label).toTypedArray()

    companion object {
        @JsExport.Ignore
        fun of(
            labellings: List<LabelledArgument>,
            attacks: List<Attack>,
            supports: List<Support>,
        ) = Graph(labellings.map { it.argument }, attacks, supports).also {
            it.labellings = labellings
        }
    }
}
