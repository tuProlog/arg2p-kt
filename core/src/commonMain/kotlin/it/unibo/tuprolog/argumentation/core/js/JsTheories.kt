@file:Suppress("NON_EXPORTABLE_TYPE")

package it.unibo.tuprolog.argumentation.core.js

import it.unibo.tuprolog.argumentation.core.runtime.theory.RuleAnnotations
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.theory.Theory
import kotlin.js.JsExport

/** The theories of tuProlog for JS: parsing, and their clauses */
@JsExport
object JsTheories {
    /** The theory written in [text], with the operators of arg2p theories (see [JsArg2p.operators]) */
    fun parse(text: String): Theory = RuleAnnotations.parse(text, JsArg2p.operators)

    fun clauses(theory: Theory): Array<Term> = theory.clauses.toList().toTypedArray()

    fun format(theory: Theory): String = theory.toString(asPrologText = true)
}
