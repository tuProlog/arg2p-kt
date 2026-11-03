package it.unibo.tuprolog.argumentation.core.primitives.graph.language

import it.unibo.tuprolog.argumentation.core.engine.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.core.engine.NativeCall
import it.unibo.tuprolog.argumentation.core.engine.replyTerms
import it.unibo.tuprolog.argumentation.core.engine.termArg
import it.unibo.tuprolog.solve.Signature

/**
 * ruleIds(+Rules, ?Ids): Ids are the identifiers of Rules, the lists `[Id, ...]` the parser reads, in their order: a
 * single visit of them, which a Prolog loop over the list would make quadratic (each step applying its bindings to the
 * rest of the list)
 */
internal object RuleIds : AbstractNativePrimitive(Signature("ruleIds", 2)) {
    override fun call(call: NativeCall): Int =
        call.replyTerms(
            1,
            call
                .termArg(0)
                .asList()!!
                .toList()
                .mapNotNull { it.asList()?.toList()?.firstOrNull() },
        )
}
