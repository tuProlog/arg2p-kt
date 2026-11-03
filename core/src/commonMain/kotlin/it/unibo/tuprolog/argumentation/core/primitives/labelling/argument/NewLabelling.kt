package it.unibo.tuprolog.argumentation.core.primitives.labelling.argument

import it.unibo.tuprolog.argumentation.core.engine.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.core.engine.NativeCall
import it.unibo.tuprolog.argumentation.core.engine.termArg
import it.unibo.tuprolog.argumentation.core.engine.termsArg
import it.unibo.tuprolog.argumentation.core.primitives.graph.attackGraph
import it.unibo.tuprolog.argumentation.core.primitives.graph.labelArguments
import it.unibo.tuprolog.argumentation.core.primitives.graph.labelledIds
import it.unibo.tuprolog.argumentation.core.primitives.graph.storedArguments
import it.unibo.tuprolog.argumentation.core.runtime.context.contextBranch
import it.unibo.tuprolog.solve.Signature

/**
 * newLabelling(+Branch, +Ids): in a new branch of Branch (left active), the arguments of Ids IN, the unlabelled
 * arguments attacked by an IN one OUT, and the other unlabelled ones UND, asserted with a single context_assert_all
 */
internal object NewLabelling : AbstractNativePrimitive(Signature("newLabelling", 2)) {
    override fun call(call: NativeCall): Int {
        if (!call.contextBranch(call.termArg(0), signature)) return NativeCall.FAIL
        val set = call.termsArg(1, signature).toSet()
        val arguments = call.storedArguments()
        val labelled = call.labelledIds("in") + call.labelledIds("out") + call.labelledIds("und")
        val newIn = arguments.filter { (id, _) -> id in set && id !in labelled }
        val inIds = call.labelledIds("in") + newIn.map { it.first }
        val attackers = call.attackGraph().attackers
        val (out, und) =
            arguments
                .filter { (id, _) -> id !in labelled && id !in inIds }
                .partition { (id, _) -> attackers[id].orEmpty().any { it in inIds } }
        call.labelArguments(listOf(Pair("in", newIn), Pair("out", out), Pair("und", und)))
        return NativeCall.CONTINUE
    }
}
