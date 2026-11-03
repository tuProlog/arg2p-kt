package it.unibo.tuprolog.argumentation.core.primitives.utils

import it.unibo.tuprolog.argumentation.core.engine.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.core.engine.NativeCall
import it.unibo.tuprolog.argumentation.core.engine.termArg
import it.unibo.tuprolog.argumentation.core.engine.termsArg
import it.unibo.tuprolog.argumentation.core.runtime.Arg2pUnificator
import it.unibo.tuprolog.argumentation.core.runtime.modules.haltsRaised
import it.unibo.tuprolog.argumentation.core.runtime.renamedCopy
import it.unibo.tuprolog.core.Tuple
import it.unibo.tuprolog.solve.Signature

/**
 * forEach(?Element, +List, +Goal): Goal (all its solutions) for each element of List, as
 * findall(_, (member(Element, List), Goal), _), but without List in the goals of the iterations: with variable
 * tracking, every resolution step visits the terms of the goal it belongs to, so the list would make every step
 * as costly as it is long. Goal runs in this module: other modules are called through ::.
 */
internal object ForEach : AbstractNativePrimitive(Signature("forEach", 3)) {
    override fun call(call: NativeCall): Int {
        val items = call.termsArg(1, signature)
        val element = call.termArg(0)
        val goal = call.termArg(2)
        // the iterations are solved one after the other, by the same solver
        val solver = call.context.createSolver()
        items.forEach { item ->
            val iteration = Tuple.of(element, goal).renamedCopy()
            val unifier = Arg2pUnificator.mgu(iteration[0], item)
            if (unifier.isSuccess) {
                solver.solve(iteration[1].apply(unifier).castToStruct()).haltsRaised().forEach { }
            }
        }
        return NativeCall.CONTINUE
    }
}
