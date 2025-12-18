package it.unibo.tuprolog.argumentation.core.libs.language

import it.unibo.tuprolog.argumentation.core.libs.ArgLibrary
import it.unibo.tuprolog.argumentation.core.libs.ArgsFlag
import it.unibo.tuprolog.argumentation.core.libs.LazyRawPrologContent
import it.unibo.tuprolog.argumentation.core.libs.Loadable
import it.unibo.tuprolog.argumentation.core.libs.basic.DynamicLoader
import it.unibo.tuprolog.core.Clause
import it.unibo.tuprolog.core.List
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Substitution
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.Var
import it.unibo.tuprolog.core.operators.Operator
import it.unibo.tuprolog.core.operators.OperatorSet
import it.unibo.tuprolog.core.operators.Specifier
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.core.toTerm
import it.unibo.tuprolog.dsl.solve.LogicProgrammingScope
import it.unibo.tuprolog.dsl.solve.prolog
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.library.Library
import it.unibo.tuprolog.solve.primitive.BinaryRelation
import it.unibo.tuprolog.solve.primitive.PrimitiveWrapper.Companion.ensuringArgumentIsVariable
import it.unibo.tuprolog.solve.primitive.Solve
import it.unibo.tuprolog.solve.primitive.UnaryPredicate
import kotlin.random.Random
import kotlin.collections.List as KList

abstract class RuleParserBase :
    LazyRawPrologContent(),
    ArgLibrary,
    Loadable {
    override val alias = "prolog.argumentation.parser"

    override val baseContent: Library
        get() =
            Library.of(
                alias = this.alias,
                primitives =
                    mapOf(
                        StrictRules::descriptionPair.get(),
                        Axioms::descriptionPair.get(),
                        Bps::descriptionPair.get(),
                        Premises::descriptionPair.get(),
                        DefeasibleRules::descriptionPair.get(),
                        ExtractStrictIds::descriptionPair.get(),
                        RuleToClause::descriptionPair.get(),
                        RuleToClauseNatural::descriptionPair.get(),
                    ),
                clauses = this.prologTheory,
                operators = operators(),
            )

    override val baseFlags: Iterable<ArgsFlag<*, *>>
        get() = listOf(AutoTransposition, PrologStrictCompatibility)

    override fun identifier() = "parser"

    override val theoryOperators =
        DynamicLoader
            .operators()
            .plus(operators())
            .plus(OperatorSet.DEFAULT)

    companion object {
        fun operators() =
            OperatorSet(
                Operator("=>", Specifier.XFX, 1199),
                Operator(":=>", Specifier.XFX, 1199),
                Operator(":->", Specifier.XFX, 1199),
                Operator(":", Specifier.XFX, 1001),
                Operator(":=", Specifier.XFX, 1199),
//            Operator("->", Specifier.XFX, 1050)
            )
    }
}

expect object RuleParser : RuleParserBase {
    override val prologRawTheory: String
}

object AutoTransposition : ArgsFlag<Boolean, Unit> {
    override fun predicate(): String = "autoTransposition"

    override fun default(): Boolean = false

    override fun values() {}
}

object PrologStrictCompatibility : ArgsFlag<Boolean, Unit> {
    override fun predicate(): String = "prologStrictCompatibility"

    override fun default(): Boolean = false

    override fun values() {}
}

object ConversionUtils {
    fun modifiers(
        target: Clause,
        context: Solve.Request<ExecutionContext>,
    ): Term =
        prolog {
            target.bodyItems
                .map { term ->
                    if (term.isStruct && term.asStruct()?.functor == "\\+") {
                        "~"(term.asStruct()!!.args[0])
                    } else if (context.solve("clause"(term, Var.ANONYMOUS_NAME)).first().isHalt) {
                        "prolog"(term)
                    } else {
                        term
                    }
                }.let { if (it.count() > 1) tupleOf(it) else it.first() }
        }

    fun commonMap(
        context: Solve.Request<ExecutionContext>,
        first: Term,
        force: Boolean = false,
        mapper: (Iterable<Clause>, LogicProgrammingScope) -> Term,
    ): Sequence<Substitution> {
        context.ensuringArgumentIsVariable(0)
        return prolog {
            sequenceOf(
                Substitution.of(
                    first.castToVar(),
                    if (force || context.solve(Struct.parse("prologStrictCompatibility")).first().isYes) {
                        mapper(context.context.staticKb.clauses, this)
                    } else {
                        emptyLogicList
                    },
                ),
            )
        }
    }
}

object StrictRules : UnaryPredicate.WithoutSideEffects<ExecutionContext>("prologStrictRules") {
    override fun Solve.Request<ExecutionContext>.computeAllSubstitutions(first: Term): Sequence<Substitution> =
        ConversionUtils.commonMap(this, first) { clauses, prologScope ->
            clauses
                .filter { !it.isFact }
                .map {
                    prologScope.logicListOf(
                        "rule_${Random.nextInt(0, Int.MAX_VALUE)}",
                        ConversionUtils.modifiers(it, this@computeAllSubstitutions),
                        it.head!!,
                    )
                }.toTerm()
        }
}

object Axioms : UnaryPredicate.WithoutSideEffects<ExecutionContext>("prologAxioms") {
    override fun Solve.Request<ExecutionContext>.computeAllSubstitutions(first: Term): Sequence<Substitution> =
        ConversionUtils.commonMap(this, first) { clauses, prologScope ->
            clauses
                .filter { it.isFact && !listOf(":->", "->", "=>", ":=>", ":", ":=", ",").contains(it.head!!.functor) }
                .map {
                    prologScope.logicListOf(
                        "rule_${Random.nextInt(0, Int.MAX_VALUE)}",
                        it.head!!,
                    )
                }.toTerm()
        }
}

object Premises : UnaryPredicate.WithoutSideEffects<ExecutionContext>("prologPremises") {
    override fun Solve.Request<ExecutionContext>.computeAllSubstitutions(first: Term): Sequence<Substitution> =
        ConversionUtils.commonMap(this, first) { clauses, prologScope ->
            clauses
                .filter { it.isFact && it.head!!.functor == ":=" && it.head!!.arity == 1 }
                .map {
                    prologScope.logicListOf(
                        "rule_${Random.nextInt(0, Int.MAX_VALUE)}",
                        it.head!!.args[0],
                    )
                }.toTerm()
        }
}

object DefeasibleRules : UnaryPredicate.WithoutSideEffects<ExecutionContext>("prologDefeasibleRules") {
    override fun Solve.Request<ExecutionContext>.computeAllSubstitutions(first: Term): Sequence<Substitution> =
        ConversionUtils.commonMap(this, first) { clauses, prologScope ->
            clauses
                .filter {
                    it.isFact &&
                        (
                            it.head!!.functor == ":=" ||
                                (
                                    it.head!!.functor == "," &&
                                        it.head!!
                                            .args[0]
                                            .asStruct()
                                            ?.functor == ":="
                                )
                        ) &&
                        it.head!!.arity == 2
                }.map { clause ->
                    if (clause.head!!.functor == ",") {
                        val head = clause.head!!.args[0].asStruct()
                        val term =
                            (
                                listOf(head!![1]) + (
                                    clause.head!!
                                        .args[1]
                                        .asTuple()
                                        ?.args
                                        ?: listOf(clause.head!!.args[1])
                                )
                            ).toTypedArray()
                        prologScope.clauseOf(head[0].asStruct(), *term)
                    } else {
                        prologScope.clauseOf(clause.head!!.args[0].asStruct(), clause.head!!.args[1])
                    }.let {
                        prologScope.logicListOf(
                            "rule_${Random.nextInt(0, Int.MAX_VALUE)}",
                            ConversionUtils.modifiers(it, this@computeAllSubstitutions),
                            it.head!!,
                        )
                    }
                }.toTerm()
        }
}

object Bps : UnaryPredicate.WithoutSideEffects<ExecutionContext>("bpsNew") {
    override fun Solve.Request<ExecutionContext>.computeAllSubstitutions(first: Term): Sequence<Substitution> =
        ConversionUtils.commonMap(this, first, true) { clauses, prologScope ->
            clauses
                .filter { it.isFact && it.head?.functor == "bp" }
                .map {
                    Struct.of("abstractBp", prologScope.logicListOf(it.head!!.args))
                }.toTerm()
        }
}

object ExtractStrictIds : BinaryRelation.WithoutSideEffects<ExecutionContext>("extract_id") {
    override fun Solve.Request<ExecutionContext>.computeAllSubstitutions(
        first: Term,
        second: Term,
    ): Sequence<Substitution> =
        sequenceOf(
            Substitution.of(
                second.asVar()!!,
                List.of(
                    first.asList()!!.toList().map {
                        Struct.of("strict", it.asCons()!!.head)
                    },
                ),
            ),
        )
}

object RuleToClauseNatural : BinaryRelation.WithoutSideEffects<ExecutionContext>("rule_to_clause_natural") {
    class TemplateExtractor {
        private val upperRegex = Regex("""\b[A-Z][a-zA-Z]*\b""")
        private val bracketRegex = Regex("""\[([^\]]+)]""")

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
                processInternal(it.toString()).normalized
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
                                val parsed = parseTerm(extractor, it)
                                when (it.size) {
                                    2 -> Clause.of(Struct.Companion.of("rl", parsed[1]), List.of(parsed))
                                    3 -> Clause.of(Struct.Companion.of("rl", parsed[2]), List.of(parsed))
                                    else -> throw IllegalArgumentException()
                                }
                            }
                        },
                ),
            ),
        )
    }
}

object RuleToClause : BinaryRelation.WithoutSideEffects<ExecutionContext>("rule_to_clause") {
    override fun Solve.Request<ExecutionContext>.computeAllSubstitutions(
        first: Term,
        second: Term,
    ): Sequence<Substitution> =
        sequenceOf(
            Substitution.of(
                second.asVar()!!,
                List.of(
                    first
                        .asList()!!
                        .toList()
                        .map { original ->
                            original.asList()!!.toList().let {
                                when (it.size) {
                                    2 -> Clause.of(Struct.Companion.of("rl", it[1]), original)
                                    3 -> Clause.of(Struct.Companion.of("rl", it[2]), original)
                                    else -> throw IllegalArgumentException()
                                }
                            }
                        },
                ),
            ),
        )
}
