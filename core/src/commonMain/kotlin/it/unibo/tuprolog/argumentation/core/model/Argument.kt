package it.unibo.tuprolog.argumentation.core.model

import it.unibo.tuprolog.argumentation.core.libs.data.ArgumentTerm
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.core.List as LogicList

data class Argument(
    val rules: List<RuleIdentifier>,
    val topRule: RuleIdentifier,
    val conclusion: Literal,
    val groundings: List<Literal> = emptyList(),
    val defeasibleRules: List<RuleIdentifier> = emptyList(),
    val defeasiblePremises: List<RuleIdentifier> = emptyList(),
    val lastDefeasibleRules: List<RuleIdentifier> = emptyList(),
    private val term: Term? = null,
) {
    override fun hashCode(): Int = term.hashCode()

    var identifier: String = ""
    val supports: MutableList<Argument> = mutableListOf()

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

    fun termRepresentation(): Term =
        term ?: Struct.parse(
            "[$rules, $topRule, [$conclusion], $groundings, [$lastDefeasibleRules, $defeasibleRules, $defeasiblePremises]]",
        )

    override fun toString(): String = "argument(${this.termRepresentation()})"

    fun toTerm(): Term = Struct.parse(this.toString())

    companion object {
        /** The argument of [term], the term of an argument (see [ArgumentTerm]) */
        fun of(term: Term): Argument {
            val parts = ArgumentTerm.parts(term) ?: throw IllegalArgumentException("Not the term of an argument: $term")
            return Argument(
                parts.rules.names(),
                parts.topRule.toString(),
                parts.claim.toString(),
                parts.body.names(),
                parts.defeasibleRules.names(),
                parts.defeasiblePremises.names(),
                parts.lastDefeasibleRules.names(),
                term,
            )
        }

        // The items of a list, as text
        private fun Term.names(): List<String> = (this as LogicList).toList().map { it.toString() }
    }
}
