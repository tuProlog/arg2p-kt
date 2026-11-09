package it.unibo.tuprolog.argumentation.core.modules.labelling.argument

import it.unibo.tuprolog.argumentation.core.context.contextBranch
import it.unibo.tuprolog.argumentation.core.graph.attackGraph
import it.unibo.tuprolog.argumentation.core.graph.graphArguments
import it.unibo.tuprolog.argumentation.core.graph.idsArg
import it.unibo.tuprolog.argumentation.core.graph.labelArguments
import it.unibo.tuprolog.argumentation.core.graph.labelledIds
import it.unibo.tuprolog.argumentation.engine.vm.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.engine.vm.NativeCall
import it.unibo.tuprolog.argumentation.engine.vm.termArg
import it.unibo.tuprolog.argumentation.engine.words.IdSet
import it.unibo.tuprolog.solve.Signature

/**
 * newLabelling(+Branch, +Ids): in a new branch of Branch (left active), the arguments of Ids IN, the unlabelled
 * arguments attacked by an IN one OUT, and the other unlabelled ones UND
 */
internal object NewLabelling : AbstractNativePrimitive(Signature("newLabelling", 2)) {
    override fun call(call: NativeCall): Int {
        if (!call.contextBranch(call.termArg(0), signature)) return NativeCall.FAIL
        val set = IdSet.of(call.idsArg(1, signature))
        val arguments = call.graphArguments()
        val alreadyIn = call.labelledIds("in")
        val alreadyOut = call.labelledIds("out")
        val alreadyUnd = call.labelledIds("und")
        val places = arguments.ids.indices
        val unlabelled = places.filter { arguments.ids[it].let { id -> id !in alreadyIn && id !in alreadyOut && id !in alreadyUnd } }
        val newIn = unlabelled.filter { arguments.ids[it] in set }
        val inIds = alreadyIn.copy()
        newIn.forEach { inIds.add(arguments.ids[it]) }
        val graph = call.attackGraph()
        val (out, und) =
            unlabelled
                .filter { arguments.ids[it] !in inIds }
                .partition { place -> graph.attackers(arguments.ids[place]).any { it in inIds } }
        call.labelArguments(arguments, "in", newIn.toIntArray())
        call.labelArguments(arguments, "out", out.toIntArray())
        call.labelArguments(arguments, "und", und.toIntArray())
        return NativeCall.CONTINUE
    }
}
