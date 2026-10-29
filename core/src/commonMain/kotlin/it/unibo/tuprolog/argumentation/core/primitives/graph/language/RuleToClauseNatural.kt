package it.unibo.tuprolog.argumentation.core.primitives.graph.language

import it.unibo.tuprolog.argumentation.core.model.terms.RuleTerm
import it.unibo.tuprolog.core.List
import it.unibo.tuprolog.core.Substitution
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.primitive.BinaryRelation
import it.unibo.tuprolog.solve.primitive.Solve
import kotlin.collections.List as KList

/**
 * ruleToClauseNatural(+Rules, -Clauses): as ruleToClause/2, with naturalTerms: the statements written as text
 * are turned into terms, the words in upper case and in brackets becoming their arguments
 */
internal object RuleToClauseNatural : BinaryRelation.WithoutSideEffects<ExecutionContext>("ruleToClauseNatural") {
    class TemplateExtractor {
        private val upperRegex = Regex("""\b[A-Z][a-zA-Z]*\b""")
        private val bracketRegex = Regex("""\[([^\]]+)\]""")

        // template -> number of extracted slots
        private val knownTemplates = mutableMapOf<String, Int>()

        data class Result(
            val extracted: KList<String>,
            val normalized: String,
        )

        fun processLiteralRestricted(
            input: String,
            parser: (String) -> String,
        ): String {
            val trimmed =
                input
                    .replace("'-'", "-")
                    .replace("'~'", "~")
                    .trim()

            val regex = Regex("""^(-)?(~)?\(?(o|p)?\(?(-)?\(?([^()]+)\)?\)?\)?$""")

            val match = regex.matchEntire(trimmed) ?: return parser(trimmed)
            val atomText = match.groups[5]!!.value

            return trimmed.replace(atomText, parser(atomText))
        }

        fun process(text: String): String =
            processLiteralRestricted(text) {
                processInternal(it).normalized
            }

        fun processInternal(text: String): Result {
            val text =
                text
                    .trim()
                    .removeSurrounding("'")
                    .removeSurrounding("\"")

            if (text.contains("(")) {
                return Result(emptyList(), text)
            }

            // 1. Uppercase extraction
            val upperWords = upperRegex.findAll(text).toList()
            if (upperWords.isNotEmpty()) {
                val upperMatches = upperWords.map { it.value }
                val predicate = buildPredicate(text, upperMatches)
                val template = buildTemplate(text, upperWords)
                knownTemplates[template] = upperMatches.size
                return Result(upperMatches, predicate)
            }

            // 2. Bracket extraction
            val bracketMatches = bracketRegex.findAll(text).map { it.groupValues[1] }.toList()
            if (bracketMatches.isNotEmpty()) {
                val predicate = buildPredicate(text.replace("[", "").replace("]", ""), bracketMatches)
                return Result(bracketMatches, predicate)
            }

            // 3. Template matching
            for ((template, _) in knownTemplates) {
                val regex = Regex(template)
                val match = regex.matchEntire(text)
                if (match != null) {
                    val extracted = match.groupValues.drop(1)
                    val res = buildPredicate(text, extracted)
                    return Result(extracted, res)
                }
            }

            // 4. Fallback
            return Result(emptyList(), text.replace(" ", "_"))
        }

        private fun buildPredicate(
            text: String,
            extracted: KList<String>,
        ): String {
            var tmp = text

            // Remove extracted elements
            extracted.forEach {
                tmp = tmp.replaceFirst(Regex("""\b${Regex.escape(it)}\b"""), "")
            }

            // Normalize predicate
            val predicate =
                tmp
                    .trim()
                    .replace(Regex("""\s+"""), "_")

            // Build arguments
            val args = extracted.joinToString(", ")

            return "$predicate($args)"
        }

        // The pattern of the texts written as [text], with other words in place of [words]: the rest of the text, the
        // user's, matched as it is (escaped)
        private fun buildTemplate(
            text: String,
            words: KList<MatchResult>,
        ): String {
            var last = 0
            val template = StringBuilder("^")
            words.forEach { word ->
                template.append(Regex.escape(text.substring(last, word.range.first))).append("(.+?)")
                last = word.range.last + 1
            }
            return template.append(Regex.escape(text.substring(last))).append("$").toString()
        }
    }

    fun parseTerm(
        extractor: TemplateExtractor,
        target: KList<Term>,
    ): KList<Term> {
        val mapped =
            target.map { term ->
                if (term.isTuple) {
                    term.asTuple()!!.toList().joinToString(",", prefix = "(", postfix = ")") {
                        extractor.process(it.toString())
                    }
                } else {
                    extractor.process(term.toString())
                }
            }
        return Term.parse("[${mapped.joinToString(",")}]").asList()!!.toList()
    }

    override fun Solve.Request<ExecutionContext>.computeAllSubstitutions(
        first: Term,
        second: Term,
    ): Sequence<Substitution> {
        val extractor = TemplateExtractor()
        return sequenceOf(
            Substitution.of(
                second.asVar()!!,
                List.of(
                    first
                        .asList()!!
                        .toList()
                        .map { original ->
                            original.asList()!!.toList().let {
                                RuleTerm.of(List.of(parseTerm(extractor, it))) ?: throw IllegalArgumentException()
                            }
                        },
                ),
            ),
        )
    }
}
