package it.unibo.tuprolog.argumentation.core.libraries.language.plainProlog.primitives

import it.unibo.tuprolog.argumentation.core.libraries.framework.parser.helpers.RuleAnnotations
import it.unibo.tuprolog.argumentation.core.libraries.framework.parser.helpers.TheoryClauses
import it.unibo.tuprolog.argumentation.core.libraries.language.plainProlog.helpers.ClauseConversion
import it.unibo.tuprolog.argumentation.engine.vm.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.engine.vm.NativeCall
import it.unibo.tuprolog.core.Atom
import it.unibo.tuprolog.core.toTerm
import it.unibo.tuprolog.solve.Signature

/**
 * prologDefeasibleRules(-Rules): the Prolog clauses of the theory (Head :- Body) with an identifier (see
 * [RuleAnnotations]) as defeasible rules, `[Id, Body, Head]`
 */
internal object PrologDefeasibleRules : AbstractNativePrimitive(Signature("prologDefeasibleRules", 1)) {
    override fun call(call: NativeCall): Int {
        val context = call.context
        return TheoryClauses.reply(call, signature) { clauses, prologScope ->
            clauses
                .filter { (_, clause) -> clause.isRule }
                .mapNotNull { (_, clause) ->
                    RuleAnnotations.of(clause)?.let {
                        prologScope.logicListOf(Atom.of(it), ClauseConversion.modifiers(clause, context), clause.head!!)
                    }
                }.toTerm()
        }
    }
}
