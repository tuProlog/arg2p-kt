package it.unibo.tuprolog.argumentation.core.libraries.graph.structured.extension.constraints.primitives

import it.unibo.tuprolog.argumentation.core.libraries.graph.structured.extension.constraints.helpers.LinearSystem
import it.unibo.tuprolog.argumentation.core.libraries.graph.structured.extension.constraints.helpers.LinearTerms
import it.unibo.tuprolog.argumentation.engine.natives.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.engine.natives.NativeCall
import it.unibo.tuprolog.argumentation.engine.natives.reply
import it.unibo.tuprolog.argumentation.engine.natives.termArg
import it.unibo.tuprolog.core.List
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.exception.error.TypeError

/** linear_constraint(+C): C is a constraint the module decides, a comparison of linear expressions (see LinearTerms) */
internal object LinearConstraint : AbstractNativePrimitive(Signature("linear_constraint", 1)) {
    override fun call(call: NativeCall): Int = call.reply(LinearTerms.isLinear(call.termArg(0)))
}

/**
 * linear_satisfiable(+Constraints): the linear constraints of the list Constraints (see linear_constraint/1) can hold
 * together, their variables ranging over the rationals; the bound ones are checked by their values
 */
internal object LinearSatisfiable : AbstractNativePrimitive(Signature("linear_satisfiable", 1)) {
    override fun call(call: NativeCall): Int {
        val list =
            call.termArg(0) as? List ?: throw TypeError.forArgument(call.context, signature, TypeError.Expected.LIST, call.termArg(0), 0)
        val constraints = list.toList()
        val other = constraints.firstOrNull { !LinearTerms.isLinear(it) }
        if (other != null) throw TypeError.forArgument(call.context, signature, TypeError.Expected.EVALUABLE, other, 0)
        return call.reply(LinearSystem.satisfiable(LinearTerms.rows(constraints)))
    }
}
