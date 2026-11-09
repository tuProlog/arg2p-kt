package it.unibo.tuprolog.argumentation.core.libraries.framework.loader.primitives

import it.unibo.tuprolog.argumentation.core.libraries.framework.loader.DynamicLoader
import it.unibo.tuprolog.argumentation.engine.vm.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.engine.vm.NativeCall
import it.unibo.tuprolog.argumentation.engine.vm.reply
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.exception.error.TypeError

/**
 * module_defines(+Module, +Name/Arity): whether Module, one of the solver, defines the predicate Name/Arity, as a
 * clause or a primitive of its own. The phases ask it of the modules the flags select before calling them: a hook a
 * module may leave out (queryDependencies/2) is told apart from one it must define (modifyArgumentationGraph/0), rather
 * than both failing alike as unknown predicates do. The module is not loaded for it.
 */
internal class ModuleDefines(
    private val loader: DynamicLoader,
) : AbstractNativePrimitive(Signature("module_defines", 2)) {
    override fun call(call: NativeCall): Int {
        val nameWord = call.arg(0)
        if (call.kind(nameWord) != NativeCall.ATOM) {
            throw TypeError.forArgument(call.context, signature, TypeError.Expected.ATOM, call.term(nameWord), 0)
        }
        val indicator = call.arg(1)
        val isIndicator =
            call.kind(indicator) == NativeCall.STRUCT &&
                call.functorName(indicator) == "/" &&
                call.arityOf(indicator) == 2 &&
                call.kind(call.argOf(indicator, 0)) == NativeCall.ATOM &&
                call.kind(call.argOf(indicator, 1)) == NativeCall.INT
        if (!isIndicator) {
            throw TypeError.forArgument(call.context, signature, TypeError.Expected.PREDICATE_INDICATOR, call.term(indicator), 1)
        }
        val predicate = "${call.atomName(call.argOf(indicator, 0))}/${call.intValue(call.argOf(indicator, 1))}"
        return call.reply(predicate in loader.modules.definedBy(call.atomName(nameWord)) { call.context })
    }
}
