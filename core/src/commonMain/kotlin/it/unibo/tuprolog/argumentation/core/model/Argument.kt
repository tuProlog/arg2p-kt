package it.unibo.tuprolog.argumentation.core.model

import it.unibo.tuprolog.core.Cons
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.parsing.parse

data class Argument(
    val rules: List<Rule>,
    val topRule: Rule,
    val conclusion: Literal,
    val groundings: List<Literal> = emptyList(),
    val defeasibleRules: List<Rule> = emptyList(),
    val defeasiblePremises: List<Rule> = emptyList(),
    val lastDefeasibleRules: List<Rule> = emptyList()
) {

    var identifier: String = ""
    val supports: MutableList<Argument> = mutableListOf()

    val descriptor: String
        get() = "$identifier : " + (
            if (topRule == "none") rules.firstOrNull() ?: "" else {
                supports.map { it.identifier }.plus(topRule)
                    .reduce { a: String, b: String -> "$a,$b" }
            }
            ) + " : " + conclusion

    fun termRepresentation(): Term = Struct.parse("[$rules, $topRule, $conclusion, $groundings, [$lastDefeasibleRules, $defeasibleRules, $defeasiblePremises]]")

    override fun toString(): String {
        return "argument(${this.termRepresentation()})"
    }

    companion object {

        fun of(term: Term): Argument {
            fun termAsList(t: Term?): List<Term> = (t as Cons).toList()

            fun toStringList(argument: List<Term>, target: Int): List<String> {
                return (
                    if (argument[target].isEmptyList) emptyList()
                    else (argument[target] as Cons).toList()
                    ).map { x -> x.toString() }
            }

            fun argRules(argument: List<Term>): List<String> = toStringList(argument, 0)

            fun argTopRule(argument: List<Term>): String = argument[1].toString()

            fun argConclusion(argument: List<Term>): String = argument[2].toString()

            fun argGroundings(argument: List<Term>): List<String> = toStringList(argument, 3)

            fun argDefeasibleRules(argument: List<Term>): List<String> =
                toStringList(termAsList(argument[4]), 1)

            fun argDefeasiblePremises(argument: List<Term>): List<String> =
                toStringList(termAsList(argument[4]), 2)

            fun argLastDefeasibleRules(argument: List<Term>): List<String> =
                toStringList(termAsList(argument[4]), 0)

            return termAsList(term).let {
                Argument(
                    argRules(it),
                    argTopRule(it),
                    argConclusion(it),
                    argGroundings(it),
                    argDefeasibleRules(it),
                    argDefeasiblePremises(it),
                    argLastDefeasibleRules(it)
                )
            }
        }
    }
}
