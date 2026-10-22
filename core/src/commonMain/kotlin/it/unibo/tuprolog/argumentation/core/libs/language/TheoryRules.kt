package it.unibo.tuprolog.argumentation.core.libs.language

import it.unibo.tuprolog.core.List
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Substitution
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.toTerm
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.primitive.BinaryRelation
import it.unibo.tuprolog.solve.primitive.Solve
import it.unibo.tuprolog.solve.primitive.UnaryPredicate

// The rules of a theory, as the parser reads them (see convertAllRules/1)

object StrictRules : UnaryPredicate.WithoutSideEffects<ExecutionContext>("prologStrictRules") {
    override fun Solve.Request<ExecutionContext>.computeAllSubstitutions(first: Term): Sequence<Substitution> =
        ConversionUtils.commonMap(this, first) { clauses, prologScope ->
            clauses
                .filter { !it.value.isFact }
                .map { (index, clause) ->
                    prologScope.logicListOf(
                        ConversionUtils.ruleId(index),
                        ConversionUtils.modifiers(clause, this@computeAllSubstitutions),
                        clause.head!!,
                    )
                }.toTerm()
        }
}

object Axioms : UnaryPredicate.WithoutSideEffects<ExecutionContext>("prologAxioms") {
    override fun Solve.Request<ExecutionContext>.computeAllSubstitutions(first: Term): Sequence<Substitution> =
        ConversionUtils.commonMap(this, first) { clauses, prologScope ->
            clauses
                .filter { it.value.isFact && !listOf(":->", "->", "=>", ":=>", ":", ":=", ",").contains(it.value.head!!.functor) }
                .map { (index, clause) -> prologScope.logicListOf(ConversionUtils.ruleId(index), clause.head!!) }
                .toTerm()
        }
}

object Premises : UnaryPredicate.WithoutSideEffects<ExecutionContext>("prologPremises") {
    override fun Solve.Request<ExecutionContext>.computeAllSubstitutions(first: Term): Sequence<Substitution> =
        ConversionUtils.commonMap(this, first) { clauses, prologScope ->
            clauses
                .filter { it.value.isFact && it.value.head!!.functor == ":=" && it.value.head!!.arity == 1 }
                .map { (index, clause) -> prologScope.logicListOf(ConversionUtils.ruleId(index), clause.head!!.args[0]) }
                .toTerm()
        }
}

object DefeasibleRules : UnaryPredicate.WithoutSideEffects<ExecutionContext>("prologDefeasibleRules") {
    override fun Solve.Request<ExecutionContext>.computeAllSubstitutions(first: Term): Sequence<Substitution> =
        ConversionUtils.commonMap(this, first) { clauses, prologScope ->
            clauses
                .filter { (_, clause) ->
                    clause.isFact &&
                        (
                            clause.head!!.functor == ":=" ||
                                (
                                    clause.head!!.functor == "," &&
                                        clause.head!!
                                            .args[0]
                                            .asStruct()
                                            ?.functor == ":="
                                )
                        ) &&
                        clause.head!!.arity == 2
                }.map { (index, clause) ->
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
                            ConversionUtils.ruleId(index),
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
                .filter { it.value.isFact && it.value.head?.functor == "bp" }
                .map { Struct.of("abstractBp", prologScope.logicListOf(it.value.head!!.args)) }
                .toTerm()
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
