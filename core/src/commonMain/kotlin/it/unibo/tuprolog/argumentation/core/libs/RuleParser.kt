package it.unibo.tuprolog.argumentation.core.libs

import it.unibo.tuprolog.argumentation.core.Sources
import it.unibo.tuprolog.core.*
import it.unibo.tuprolog.core.operators.Operator
import it.unibo.tuprolog.core.operators.OperatorSet
import it.unibo.tuprolog.core.operators.Specifier
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.dsl.theory.PrologScopeWithTheories
import it.unibo.tuprolog.dsl.theory.prolog
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.library.AliasedLibrary
import it.unibo.tuprolog.solve.library.Library
import it.unibo.tuprolog.solve.primitive.PrimitiveWrapper.Companion.ensuringArgumentIsVariable
import it.unibo.tuprolog.solve.primitive.Solve
import it.unibo.tuprolog.solve.primitive.UnaryPredicate
import kotlin.random.Random

object RuleParser : AliasedLibrary by
Library.aliased(
    alias = "prolog.argumentation.parser",
    primitives = mapOf(
        StrictRules::descriptionPair.get(),
        Axioms::descriptionPair.get(),
        Bps::descriptionPair.get(),
        Premises::descriptionPair.get(),
        DefeasibleRules::descriptionPair.get()
    ),
    theory = Sources.ruleTranslator,
    operatorSet = OperatorSet(
        Operator("=>", Specifier.XFX, 1199),
        Operator(":=>", Specifier.XFX, 1199),
        Operator(":->", Specifier.XFX, 1199),
        Operator(":", Specifier.XFX, 1001),
        Operator(":=", Specifier.XFX, 1199)
    )
)


object ConversionUtils {
    fun modifiers(target: Clause, context: Solve.Request<ExecutionContext>): Term =
        prolog {
            target.bodyItems.map { term ->
                if (term.isStruct && term.asStruct()?.functor == "\\+") "~"(term.asStruct()!!.args[0])
                else if (context.solve("clause"(term, Var.ANONYMOUS_NAME)).first().isHalt) "prolog"(term)
                else term
            }.let { if (it.count() > 1) tupleOf(it) else it.first() }
        }

    fun commonMap(
        context: Solve.Request<ExecutionContext>,
        first: Term,
        force: Boolean = false,
        mapper: (Iterable<Clause>, PrologScopeWithTheories) -> Term
    ): Sequence<Substitution> {
        context.ensuringArgumentIsVariable(0)
        return prolog {
            sequenceOf(
                Substitution.of(
                    first.castToVar(),
                    if (force || context.solve(Struct.parse("prologStrictCompatibility")).first().isYes) {
                        mapper(context.context.staticKb.clauses, this)
                    } else {
                        emptyList
                    }
                )
            )
        }
    }
}

object StrictRules : UnaryPredicate.WithoutSideEffects<ExecutionContext>("prologStrictRules") {
    override fun Solve.Request<ExecutionContext>.computeAllSubstitutions(first: Term): Sequence<Substitution> =
        ConversionUtils.commonMap(this, first) { clauses, prologScope ->
            clauses.filter { !it.isFact }
                .map {
                    prologScope.listOf(
                        "rule_${Random.nextInt(0, Int.MAX_VALUE)}",
                        ConversionUtils.modifiers(it, this@computeAllSubstitutions),
                        it.head!!
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
                    prologScope.listOf(
                        "rule_${Random.nextInt(0, Int.MAX_VALUE)}",
                        it.head!!
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
                    prologScope.listOf(
                        "rule_${Random.nextInt(0, Int.MAX_VALUE)}",
                        it.head!!.args[0]
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
                        val term = (
                                listOf(head!![1]) + (
                                        clause.head!!.args[1].asTuple()?.args
                                            ?: listOf(clause.head!!.args[1])
                                        )
                                ).toTypedArray()
                        prologScope.clauseOf(head[0].asStruct(), *term)
                    } else {
                        prologScope.clauseOf(clause.head!!.args[0].asStruct(), clause.head!!.args[1])
                    }.let {
                        prologScope.listOf(
                            "rule_${Random.nextInt(0, Int.MAX_VALUE)}",
                            ConversionUtils.modifiers(it, this@computeAllSubstitutions),
                            it.head!!
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
                    prologScope.listOf(
                        "bps",
                        it.head!!
                    )
                }.toTerm()
        }
}

