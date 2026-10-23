package it.unibo.tuprolog.argumentation.core.js

import kotlin.js.JsExport

/** An argumentation graph for JS: its arguments, labelled, and the attacks between them */
@JsExport
class JsGraph internal constructor(
    val arguments: Array<JsArgument>,
    val attacks: Array<JsAttack>,
) {
    override fun equals(other: Any?): Boolean =
        other is JsGraph && arguments.contentEquals(other.arguments) && attacks.contentEquals(other.attacks)

    override fun hashCode(): Int = 31 * arguments.contentHashCode() + attacks.contentHashCode()
}
