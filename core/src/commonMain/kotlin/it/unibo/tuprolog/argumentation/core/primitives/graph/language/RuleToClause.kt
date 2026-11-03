package it.unibo.tuprolog.argumentation.core.primitives.graph.language

import it.unibo.tuprolog.argumentation.core.engine.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.core.engine.NativeCall
import it.unibo.tuprolog.argumentation.core.engine.replyTerms
import it.unibo.tuprolog.argumentation.core.engine.termArg
import it.unibo.tuprolog.argumentation.core.model.terms.RuleTerm
import it.unibo.tuprolog.solve.Signature

/**
 * ruleToClause(+Rules, -Clauses): the rules and premises of the parser (`[Id, Body, Head]`, `[Id, Head]`) as
 * the clauses of the module rule
 */
internal object RuleToClause : AbstractNativePrimitive(Signature("ruleToClause", 2)) {
    override fun call(call: NativeCall): Int =
        call.replyTerms(
            1,
            call
                .termArg(0)
                .asList()!!
                .toList()
                .map { original -> RuleTerm.of(original.castToList()) ?: throw IllegalArgumentException() },
        )
}
