package it.unibo.tuprolog.argumentation.core.primitives.context

import it.unibo.tuprolog.argumentation.core.engine.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.core.engine.NativeCall
import it.unibo.tuprolog.argumentation.core.engine.termArg
import it.unibo.tuprolog.argumentation.core.runtime.context.ContextBranches
import it.unibo.tuprolog.solve.Signature

/** context_unifiable(Head, Body): Body of the stored clauses whose head unifies with Head, without binding them */
internal class ContextUnifiable(
    private val branches: ContextBranches,
) : AbstractNativePrimitive(Signature("context_unifiable", 2)) {
    override fun call(call: NativeCall): Int =
        call.answersOf(branches.selected.unifiableBodies(call.termArg(0), call.termArg(1)).iterator())
}
