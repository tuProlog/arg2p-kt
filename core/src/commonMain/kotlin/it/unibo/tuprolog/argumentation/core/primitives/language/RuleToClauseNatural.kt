package it.unibo.tuprolog.argumentation.core.primitives.language

import it.unibo.tuprolog.argumentation.core.model.terms.RuleTerm
import it.unibo.tuprolog.core.List
import it.unibo.tuprolog.core.Substitution
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.primitive.BinaryRelation
import it.unibo.tuprolog.solve.primitive.Solve
import kotlin.collections.List as KList

object RuleToClauseNatural : BinaryRelation.WithoutSideEffects<ExecutionContext>("rule_to_clause_natural") {
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
            val upperMatches = upperRegex.findAll(text).map { it.value }.toList()
            if (upperMatches.isNotEmpty()) {
                val predicate = buildPredicate(text, upperMatches)
                val template = buildTemplate(text, upperMatches)
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

        private fun buildTemplate(
            text: String,
            extracted: KList<String>,
        ): String {
            var template = text
            extracted.forEach { template = template.replace(it, "(.+?)") }
            return "^$template$"
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
