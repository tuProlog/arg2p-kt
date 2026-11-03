package it.unibo.tuprolog.argumentation.core.primitives.graph.language

import it.unibo.tuprolog.argumentation.core.engine.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.core.engine.NativeCall
import it.unibo.tuprolog.argumentation.core.engine.listArg
import it.unibo.tuprolog.argumentation.core.engine.replyItems
import it.unibo.tuprolog.solve.Signature

/**
 * ruleIds(+Rules, ?Ids): Ids are the identifiers of Rules, the lists `[Id, ...]` the parser reads, in their order: a
 * single visit of them, which a Prolog loop over the list would make quadratic (each step applying its bindings to the
 * rest of the list)
 */
internal object RuleIds : AbstractNativePrimitive(Signature("ruleIds", 2)) {
    override fun call(call: NativeCall): Int {
        val ids = call.listArg(0, signature).filter { call.kind(it) == NativeCall.LIST }.map { call.argOf(it, 0) }
        return call.replyItems(1, ids.toIntArray())
    }
}
