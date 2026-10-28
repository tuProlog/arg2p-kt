@file:Suppress("NON_EXPORTABLE_TYPE")

package it.unibo.tuprolog.argumentation.core.model

import it.unibo.tuprolog.argumentation.core.model.terms.ArgumentTerm
import it.unibo.tuprolog.argumentation.core.model.terms.StatementTerm
import it.unibo.tuprolog.argumentation.core.model.terms.TermValues
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.parsing.parse
import kotlin.js.JsExport
import kotlin.js.JsName
import it.unibo.tuprolog.core.List as LogicList

/**
 * An argument of a graph: the [rules] it is made of, its [topRule] (none for a premise) and its [conclusion], with the
 * arguments it is built on ([supports]), identified as A0, A1, ... in the graph ([identifier]); [descriptor] writes it
 * as `A2 : A0,r1 : c`
 */
@JsExport
data class Argument(
    @JsExport.Ignore @JsName("rulesList") val rules: List<RuleIdentifier>,
    val topRule: RuleIdentifier,
    val conclusion: Literal,
    @JsExport.Ignore val groundings: List<Literal> = emptyList(),
    @JsExport.Ignore val defeasibleRules: List<RuleIdentifier> = emptyList(),
    @JsExport.Ignore val defeasiblePremises: List<RuleIdentifier> = emptyList(),
    @JsExport.Ignore val lastDefeasibleRules: List<RuleIdentifier> = emptyList(),
    private val term: Term? = null,
) {
    override fun hashCode(): Int = term.hashCode()

    var identifier: String = ""

    @JsExport.Ignore
    @JsName("supportsList")
    val supports: MutableList<Argument> = mutableListOf()

    /** The rules of the argument (an array in JavaScript) */
    @JsName("rules")
    val ruleArray: Array<RuleIdentifier> get() = rules.toTypedArray()

    /** The arguments the argument is built on (an array in JavaScript) */
    @JsName("supports")
    val supportArray: Array<Argument> get() = supports.toTypedArray()

    val descriptor: String
        get() =
            "$identifier : " + (
                if (topRule == "none") {
                    rules.firstOrNull() ?: ""
                } else {
                    supports
                        .map { it.identifier }
                        .plus(topRule)
                        .reduce { a: String, b: String -> "$a,$b" }
                }
            ) + " : " + conclusion

    /** The term of the argument (see [ArgumentTerm]) */
    @JsExport.Ignore
    fun termRepresentation(): Term =
        term ?: ArgumentTerm.of(
            rules = rules.asTerms(),
            topRule = Term.parse(topRule),
            conclusion = StatementTerm.of(Term.parse(conclusion, TermValues.operators)),
            body = groundings.asTerms(),
            info = ArgumentTerm.info(lastDefeasibleRules.asTerms(), defeasibleRules.asTerms(), defeasiblePremises.asTerms()),
        )

    override fun toString(): String = toTerm().toString()

    /** The argument as the context keeps it, argument(Argument) */
    @JsExport.Ignore
    fun toTerm(): Term = Struct.of("argument", termRepresentation())

    companion object {
        /** The argument of [term], the term of an argument (see [ArgumentTerm]) */
        @JsExport.Ignore
        fun of(term: Term): Argument {
            val parts = ArgumentTerm.parts(term) ?: throw IllegalArgumentException("Not the term of an argument: $term")
            return Argument(
                parts.rules.names(),
                parts.topRule.toString(),
                parts.claim?.let { TermValues.format(it) }.toString(),
                parts.body.names(),
                parts.defeasibleRules.names(),
                parts.defeasiblePremises.names(),
                parts.lastDefeasibleRules.names(),
                term,
            )
        }

        // The items of a list, as text
        private fun Term.names(): List<String> = (this as LogicList).toList().map { it.toString() }

        // The list of the terms written in the strings
        private fun List<String>.asTerms(): Term = LogicList.of(map { Term.parse(it) })
    }
}
