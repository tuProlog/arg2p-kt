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

abstract class RuleParserBase : ArgLibrary, LazyRawPrologContent(), Loadable {
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
                    ),
                clauses = this.prologTheory,
                operators = operators(),
            )

    override val baseFlags: Iterable<ArgsFlag<*, *>>
        get() = listOf(AutoTransposition, PrologStrictCompatibility)

    override fun identifier() = "parser"

    override val theoryOperators =
        DynamicLoader.operators()
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

expect object RuleParser : RuleParserBase

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
            target.bodyItems.map { term ->
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
            clauses.filter { !it.isFact }
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
                    it.isFact && (
                        it.head!!.functor == ":=" ||
                            (it.head!!.functor == "," && it.head!!.args[0].asStruct()?.functor == ":=")
                    ) &&
                        it.head!!.arity == 2
                }
                .map { clause ->
                    if (clause.head!!.functor == ",") {
                        val head = clause.head!!.args[0].asStruct()
                        val term =
                            (
                                listOf(head!![1]) + (
                                    clause.head!!.args[1].asTuple()?.args
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

object RuleToClause : BinaryRelation.WithoutSideEffects<ExecutionContext>("rule_to_clause") {
    override fun Solve.Request<ExecutionContext>.computeAllSubstitutions(
        first: Term,
        second: Term,
    ): Sequence<Substitution> =
        sequenceOf(
            Substitution.of(
                second.asVar()!!,
                List.of(
                    first.asList()!!.toList()
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
