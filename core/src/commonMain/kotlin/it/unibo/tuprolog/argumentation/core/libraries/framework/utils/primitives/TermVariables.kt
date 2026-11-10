package it.unibo.tuprolog.argumentation.core.libraries.framework.utils.primitives

import it.unibo.tuprolog.argumentation.engine.vm.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.engine.vm.NativeCall
import it.unibo.tuprolog.argumentation.engine.vm.termArg
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.core.List as LogicList

/** termVariables(+Term, -Vars): Vars are the variables of Term, each once, in the order they are met (as ISO's) */
internal object TermVariables : AbstractNativePrimitive(Signature("termVariables", 2)) {
    override fun call(call: NativeCall): Int {
        val variables =
            call
                .termArg(0)
                .variables
                .distinct()
                .toList()
        return if (call.unify(call.arg(1), call.word(LogicList.of(variables)))) NativeCall.CONTINUE else NativeCall.FAIL
    }
}
