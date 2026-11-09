package it.unibo.tuprolog.argumentation.core.context

import it.unibo.tuprolog.argumentation.engine.vm.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.engine.vm.NativeCall
import it.unibo.tuprolog.solve.Signature

/** context_template_save: the active branch kept as the template context_template_restore/0 gives back (the converted theory, which every solve starts from) */
internal class ContextTemplateSave(
    private val branches: ContextBranches,
) : AbstractNativePrimitive(Signature("context_template_save", 0)) {
    override fun call(call: NativeCall): Int {
        branches.saveTemplate(call.theoryVersion)
        return NativeCall.CONTINUE
    }
}
