package it.unibo.tuprolog.argumentation.core.primitives

import it.unibo.tuprolog.core.Clause
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Substitution
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.Var
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.dsl.theory.prolog
import it.unibo.tuprolog.solve.ExecutionContext
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
}

object StrictRules1 : UnaryPredicate.WithoutSideEffects<ExecutionContext>("prologStrictRules") {
    override fun Solve.Request<ExecutionContext>.computeAllSubstitutions(first: Term): Sequence<Substitution> {
        ensuringArgumentIsVariable(0)
        return prolog {
            sequenceOf(
                Substitution.of(
                    first.castToVar(),
                    if (this@computeAllSubstitutions.solve(Struct.parse("prologStrictCompatibility")).first().isYes) {
                        this@computeAllSubstitutions.context.staticKb.clauses
                            .filter { !it.isFact }
                            .map {
                                listOf(
                                    "rule_${nextInt(0, Int.MAX_VALUE)}",
                                    ConversionUtils.modifiers(it, this@computeAllSubstitutions),
                                    it.head
                                )
                            }.toTerm()
                    } else {
                        emptyList
                    }
                )
            )
        }
    }
}

object Axioms1 : UnaryPredicate.WithoutSideEffects<ExecutionContext>("prologAxioms") {
    override fun Solve.Request<ExecutionContext>.computeAllSubstitutions(first: Term): Sequence<Substitution> {
        ensuringArgumentIsVariable(0)
        return prolog {
            sequenceOf(
                Substitution.of(
                    first.castToVar(),
                    if (this@computeAllSubstitutions.solve(Struct.parse("prologStrictCompatibility")).first().isYes) {
                        this@computeAllSubstitutions.context.staticKb.clauses
                            .filter { it.isFact && !ktListOf(":->", "->", "=>", ":=>", ":").contains(it.head!!.functor) }
                            .map {
                                listOf(
                                    "rule_${nextInt(0, Int.MAX_VALUE)}",
                                    it.head
                                )
                            }.toTerm()
                    } else {
                        emptyList
                    }
                )
            )
        }
    }
}

object Bps1 : UnaryPredicate.WithoutSideEffects<ExecutionContext>("bpsNew") {
    override fun Solve.Request<ExecutionContext>.computeAllSubstitutions(first: Term): Sequence<Substitution> {
        ensuringArgumentIsVariable(0)
        return prolog {
            sequenceOf(
                Substitution.of(
                    first.castToVar(),
                    this@computeAllSubstitutions.context.staticKb.clauses
                        .filter { it.isFact && it.head?.functor == "bp" }
                        .map {
                            listOf(
                                "bps",
                                it.head
                            )
                        }.toTerm()
                )
            )
        }
    }
}
