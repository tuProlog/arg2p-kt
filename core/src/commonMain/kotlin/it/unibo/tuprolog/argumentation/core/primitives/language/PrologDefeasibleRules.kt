package it.unibo.tuprolog.argumentation.core.primitives.language

import it.unibo.tuprolog.core.Substitution
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.toTerm
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.primitive.Solve
import it.unibo.tuprolog.solve.primitive.UnaryPredicate

/**
 * prologDefeasibleRules(-Rules): the rules of the theory written `Head := Body` as defeasible rules,
 * `[Id, Body, Head]`, with prologStrictCompatibility on (none otherwise)
 */
object PrologDefeasibleRules : UnaryPredicate.WithoutSideEffects<ExecutionContext>("prologDefeasibleRules") {
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
