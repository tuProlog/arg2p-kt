package it.unibo.tuprolog.argumentation.core.runtime.context

import it.unibo.tuprolog.argumentation.engine.vm.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.engine.vm.NativeCall
import it.unibo.tuprolog.solve.Signature

/** context_template_restore: the active branch replaced by a copy of the template saved by context_template_save/0; fails when there is none (or the theory or the flags changed since) */
internal class ContextTemplateRestore(
    private val branches: ContextBranches,
) : AbstractNativePrimitive(Signature("context_template_restore", 0)) {
    override fun call(call: NativeCall): Int = if (branches.restoreTemplate(call.theoryVersion)) NativeCall.CONTINUE else NativeCall.FAIL
}
