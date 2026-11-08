package it.unibo.tuprolog.argumentation.core.primitives.modules

import it.unibo.tuprolog.argumentation.core.runtime.modules.DynamicLoader
import it.unibo.tuprolog.argumentation.engine.vm.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.engine.vm.NativeCall
import it.unibo.tuprolog.argumentation.engine.vm.reply
import it.unibo.tuprolog.argumentation.engine.vm.termArg
import it.unibo.tuprolog.core.Atom
import it.unibo.tuprolog.core.Indicator
import it.unibo.tuprolog.core.Integer
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.exception.error.TypeError

/**
 * module_defines(+Module, +Name/Arity): whether Module, one of the solver, defines the predicate Name/Arity, as a
 * clause or a primitive. The pipeline asks it of the modules the flags select before calling them: a hook a module
 * may leave out (queryDependencies/2) is told apart from one it must define (modifyArgumentationGraph/0), rather than
 * both failing alike as unknown predicates do.
 */
internal class ModuleDefines(
    private val loader: DynamicLoader,
) : AbstractNativePrimitive(Signature("module_defines", 2)) {
    override fun call(call: NativeCall): Int {
        val nameWord = call.arg(0)
        if (call.kind(nameWord) != NativeCall.ATOM) {
            throw TypeError.forArgument(call.context, signature, TypeError.Expected.ATOM, call.term(nameWord), 0)
        }
        val name = call.atomName(nameWord)
        val indicator = call.termArg(1)
        val predicate = (indicator as? Struct)?.takeIf { it.functor == "/" && it.arity == 2 }?.get(0) as? Atom
        val arity = ((indicator as? Struct)?.takeIf { it.functor == "/" && it.arity == 2 }?.get(1) as? Integer)?.intValue?.toInt()
        if (predicate == null || arity == null) {
            throw TypeError.forArgument(call.context, signature, TypeError.Expected.PREDICATE_INDICATOR, indicator, 1)
        }
        val libraries = loader.modules.of(name) { call.context }.libraries
        val clauses =
            defined.getOrPut(name) {
                // (the clauses visited as they are: a theory indexed over all of them cost more than the first solve)
                libraries.libraries.flatMap { library -> library.clauses.mapNotNull { it.head?.indicator } }.toSet()
            }
        val primitive = Signature(predicate.value, arity)
        val isDefined = libraries.libraries.any { primitive in it.primitives } || Indicator.of(predicate.value, arity) in clauses
        return call.reply(isDefined)
    }

    // The predicates the clauses of each module define, by the name of the module: a module's libraries do not change
    private val defined = mutableMapOf<String, Set<Indicator>>()
}
