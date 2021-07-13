package it.unibo.tuprolog.argumentation.core.primitives

import it.unibo.tuprolog.core.Clause
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Substitution
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.Var
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.core.toTerm
import it.unibo.tuprolog.dsl.theory.PrologScopeWithTheories
import it.unibo.tuprolog.dsl.theory.prolog
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.primitive.PrimitiveWrapper.Companion.ensuringArgumentIsVariable
import it.unibo.tuprolog.solve.primitive.Solve
import it.unibo.tuprolog.solve.primitive.UnaryPredicate
import kotlin.random.Random.Default.nextInt

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

object StrictRules1 : UnaryPredicate.WithoutSideEffects<ExecutionContext>("prologStrictRules") {
    override fun Solve.Request<ExecutionContext>.computeAllSubstitutions(first: Term): Sequence<Substitution> =
        ConversionUtils.commonMap(this, first) { clauses, prologScope ->
            clauses.filter { !it.isFact }
                .map {
                    prologScope.listOf(
                        "rule_${nextInt(0, Int.MAX_VALUE)}",
                        ConversionUtils.modifiers(it, this@computeAllSubstitutions),
                        it.head!!
                    )
                }.toTerm()
        }
}

object Axioms1 : UnaryPredicate.WithoutSideEffects<ExecutionContext>("prologAxioms") {
    override fun Solve.Request<ExecutionContext>.computeAllSubstitutions(first: Term): Sequence<Substitution> =
        ConversionUtils.commonMap(this, first) { clauses, prologScope ->
            clauses
                .filter { it.isFact && !listOf(":->", "->", "=>", ":=>", ":", ":=", ",").contains(it.head!!.functor) }
                .map {
                    prologScope.listOf(
                        "rule_${nextInt(0, Int.MAX_VALUE)}",
                        it.head!!
                    )
                }.toTerm()
        }
}

object Premises1 : UnaryPredicate.WithoutSideEffects<ExecutionContext>("prologPremises") {
    override fun Solve.Request<ExecutionContext>.computeAllSubstitutions(first: Term): Sequence<Substitution> =
        ConversionUtils.commonMap(this, first) { clauses, prologScope ->
            clauses
                .filter { it.isFact && it.head!!.functor == ":=" && it.head!!.arity == 1 }
                .map {
                    prologScope.listOf(
                        "rule_${nextInt(0, Int.MAX_VALUE)}",
                        it.head!!.args[0]
                    )
                }.toTerm()
        }
}

object DefeasibleRules1 : UnaryPredicate.WithoutSideEffects<ExecutionContext>("prologDefeasibleRules") {
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
                            "rule_${nextInt(0, Int.MAX_VALUE)}",
                            ConversionUtils.modifiers(it, this@computeAllSubstitutions),
                            it.head!!
                        )
                    }
                }.toTerm()
        }
}

object Bps1 : UnaryPredicate.WithoutSideEffects<ExecutionContext>("bpsNew") {
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
