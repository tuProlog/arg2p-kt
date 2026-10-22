package it.unibo.tuprolog.argumentation.core.model

import it.unibo.tuprolog.argumentation.core.mining.supports
import it.unibo.tuprolog.argumentation.core.modules.data.ArgumentTerm
import it.unibo.tuprolog.argumentation.core.modules.data.StatementTerm
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

    /** The term of the argument (see [ArgumentTerm]) */
    fun termRepresentation(): Term =
        term ?: ArgumentTerm.of(
            rules = rules.asTerms(),
            topRule = Term.parse(topRule),
            conclusion = StatementTerm.of(Term.parse(conclusion)),
            body = groundings.asTerms(),
            info = ArgumentTerm.info(lastDefeasibleRules.asTerms(), defeasibleRules.asTerms(), defeasiblePremises.asTerms()),
        )

    override fun toString(): String = toTerm().toString()

    /** The argument as the context keeps it, argument(Argument) */
    fun toTerm(): Term = Struct.of("argument", termRepresentation())

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

        // The list of the terms written in the strings
        private fun List<String>.asTerms(): Term = LogicList.of(map { Term.parse(it) })
    }
}
