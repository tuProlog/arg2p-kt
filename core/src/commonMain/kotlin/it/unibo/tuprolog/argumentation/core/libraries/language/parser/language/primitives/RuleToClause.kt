package it.unibo.tuprolog.argumentation.core.libraries.language.parser.language.primitives

import it.unibo.tuprolog.argumentation.core.libraries.language.parser.helpers.RuleTerm
import it.unibo.tuprolog.argumentation.engine.vm.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.engine.vm.NativeCall
import it.unibo.tuprolog.argumentation.engine.vm.items
import it.unibo.tuprolog.argumentation.engine.vm.replyItems
import it.unibo.tuprolog.solve.Signature

/**
 * ruleToClause(+Rules, -Clauses): the rules and premises of the parser (`[Id, Body, Head]`, `[Id, Head]`) as
 * the clauses of the module rule
 */
internal object RuleToClause : AbstractNativePrimitive(Signature("ruleToClause", 2)) {
    override fun call(call: NativeCall): Int {
        val rules = call.items(call.arg(0)) ?: throw IllegalArgumentException()
        val clauses =
            IntArray(rules.size) { i ->
                val rule = rules[i]
                val parts = call.items(rule) ?: throw IllegalArgumentException()
                if (parts.size != 2 && parts.size != 3) throw IllegalArgumentException()
                call.struct(":-", intArrayOf(call.struct(RuleTerm.HEAD, intArrayOf(parts[parts.size - 1])), rule))
            }
        return call.replyItems(1, clauses)
    }
}
