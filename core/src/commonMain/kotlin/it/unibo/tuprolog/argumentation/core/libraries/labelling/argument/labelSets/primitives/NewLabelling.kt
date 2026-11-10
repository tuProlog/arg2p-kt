package it.unibo.tuprolog.argumentation.core.libraries.labelling.argument.labelSets.primitives

import it.unibo.tuprolog.argumentation.core.libraries.framework.context.helpers.contextBranch
import it.unibo.tuprolog.argumentation.core.libraries.framework.store.helpers.graph.attackGraph
import it.unibo.tuprolog.argumentation.core.libraries.framework.store.helpers.graph.graphArguments
import it.unibo.tuprolog.argumentation.core.libraries.framework.store.helpers.graph.idsArg
import it.unibo.tuprolog.argumentation.core.libraries.framework.store.helpers.graph.labelArguments
import it.unibo.tuprolog.argumentation.core.libraries.framework.store.helpers.graph.labelledIds
import it.unibo.tuprolog.argumentation.engine.natives.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.engine.natives.NativeCall
import it.unibo.tuprolog.argumentation.engine.natives.termArg
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
