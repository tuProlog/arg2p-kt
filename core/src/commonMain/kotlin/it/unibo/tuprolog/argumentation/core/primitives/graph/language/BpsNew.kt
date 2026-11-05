package it.unibo.tuprolog.argumentation.core.primitives.graph.language

import it.unibo.tuprolog.argumentation.core.engine.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.core.engine.NativeCall
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.toTerm
import it.unibo.tuprolog.solve.Signature

/**
 * bpsNew(-Bps): the burdens of persuasion of the theory, its `bp(...)` facts, as `abstractBp([...])` terms
 */
internal object BpsNew : AbstractNativePrimitive(Signature("bpsNew", 1)) {
    override fun call(call: NativeCall): Int =
        ConversionUtils.commonMap(call, signature, true) { clauses, prologScope ->
            clauses
                .filter { it.value.isFact && it.value.head?.functor == "bp" }
                .map { Struct.of("abstractBp", prologScope.logicListOf(it.value.head!!.args)) }
                .toTerm()
        }
}
