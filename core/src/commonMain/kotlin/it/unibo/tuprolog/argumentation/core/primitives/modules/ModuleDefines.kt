package it.unibo.tuprolog.argumentation.core.primitives.modules

import it.unibo.tuprolog.argumentation.core.primitives.PrimitiveWithSignature
import it.unibo.tuprolog.argumentation.core.runtime.modules.DynamicLoader
import it.unibo.tuprolog.core.Atom
import it.unibo.tuprolog.core.Indicator
import it.unibo.tuprolog.core.Integer
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.exception.error.TypeError
import it.unibo.tuprolog.solve.primitive.Solve

/**
 * module_defines(+Module, +Name/Arity): whether Module, one of the solver, defines the predicate Name/Arity, as a
 * clause or a primitive. The pipeline asks it of the modules the flags select before calling them: a hook a module
 * may leave out (queryDependencies/2) is told apart from one it must define (modifyArgumentationGraph/0), rather than
 * both failing alike as unknown predicates do.
 */
internal class ModuleDefines(
    private val loader: DynamicLoader,
) : PrimitiveWithSignature {
    override val signature = Signature("module_defines", 2)

    override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
        val name =
            request.arguments[0] as? Atom
                ?: throw TypeError.forArgument(request.context, request.signature, TypeError.Expected.ATOM, request.arguments[0], 0)
        val indicator = request.arguments[1]
        val predicate = (indicator as? Struct)?.takeIf { it.functor == "/" && it.arity == 2 }?.get(0) as? Atom
        val arity = ((indicator as? Struct)?.takeIf { it.functor == "/" && it.arity == 2 }?.get(1) as? Integer)?.intValue?.toInt()
        if (predicate == null || arity == null) {
            throw TypeError.forArgument(request.context, request.signature, TypeError.Expected.PREDICATE_INDICATOR, indicator, 1)
        }
        val libraries = loader.modules.of(request, name).libraries
        val clauses =
            defined.getOrPut(name.value) {
                libraries
                    .asTheory(request.context.unificator)
                    .clauses
                    .mapNotNull { it.head?.indicator }
                    .toSet()
            }
        val isDefined = libraries.hasPrimitive(Signature(predicate.value, arity)) || Indicator.of(predicate.value, arity) in clauses
        return sequenceOf(if (isDefined) request.replySuccess() else request.replyFail())
    }

    // The predicates the clauses of each module define, by the name of the module: a module's libraries do not change
    private val defined = mutableMapOf<String, Set<Indicator>>()
}
