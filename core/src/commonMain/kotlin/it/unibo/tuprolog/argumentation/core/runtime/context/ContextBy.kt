package it.unibo.tuprolog.argumentation.core.runtime.context

import it.unibo.tuprolog.argumentation.engine.vm.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.engine.vm.ClauseStore
import it.unibo.tuprolog.argumentation.engine.vm.MachineCall
import it.unibo.tuprolog.argumentation.engine.vm.NativeCall
import it.unibo.tuprolog.argumentation.engine.vm.attempts
import it.unibo.tuprolog.solve.Signature

// The term [w] with [key] at the place: through the cells it has, the others made with fresh variables around
private fun NativeCall.unifyAt(
    place: ClauseStore.Place,
    w0: Int,
    key: Int,
): Boolean {
    val steps = place.steps
    var w = w0
    var i = 0
    while (i < steps.size && kind(w) == NativeCall.LIST) w = argOf(w, steps[i++])
    if (i < steps.size && kind(w) != NativeCall.VAR) return false
    var built = key
    for (at in steps.size - 1 downTo i) {
        built = if (steps[at] == 0) list(intArrayOf(built), variable()) else list(intArrayOf(variable()), built)
    }
    return unify(w, built)
}

private fun NativeCall.placeOf(name: Int): ClauseStore.Place? =
    if (kind(name) ==
        NativeCall.ATOM
    ) {
        ContextSchema.key(atomName(name))
    } else {
        null
    }

/** context_by(+Name, ?Key, +Fact): the solutions of Fact in the active branch, Key at the place named Name of it (see [ContextSchema.keys]) */
internal class ContextBy(
    private val branches: ContextBranches,
) : AbstractNativePrimitive(Signature("context_by", 3)) {
    override fun call(call: NativeCall): Int {
        val place = call.placeOf(call.arg(0)) ?: return NativeCall.FAIL
        val fact = call.arg(2)
        if (call.kind(fact) != NativeCall.STRUCT ||
            !call.unifyAt(place, call.argOf(fact, place.argument), call.arg(1))
        ) {
            return NativeCall.FAIL
        }
        return if (call is MachineCall) {
            call.lookup(branches.selected, branchLookup(branches.active), 2)
        } else {
            call.inline(call.struct("context_check", intArrayOf(fact)), call.libraries, theoryVisible = false)
        }
    }
}

/**
 * context_matching(+Name, +Key, ?Fact): Fact is a fact of the active branch, as stored, whose place named Name unifies
 * with Key, which it does not bind (see [ContextSchema.keys])
 */
internal class ContextMatching(
    private val branches: ContextBranches,
) : AbstractNativePrimitive(Signature("context_matching", 3)) {
    override fun call(call: NativeCall): Int {
        val place = call.placeOf(call.arg(0)) ?: return NativeCall.FAIL
        val fact = call.arg(2)
        if (call.kind(fact) != NativeCall.STRUCT) return NativeCall.FAIL
        val pattern = call.struct(call.functorName(fact), IntArray(call.arityOf(fact)) { call.variable() })
        if (!call.unifyAt(place, call.argOf(pattern, place.argument), call.arg(1))) return NativeCall.FAIL
        val matching = call.factsMatching(pattern, branches.selected)
        return call.attempts(
            matching
                .asSequence()
                .map { stored -> { c: NativeCall -> c.unify(c.arg(2), c.recall(stored.record)) } }
                .iterator(),
        )
    }
}

/** context_stored_id(+Fact, -Id): Id is the identifier of a fact of the active branch that unifies with Fact, which it does not bind */
internal class ContextStoredId(
    private val branches: ContextBranches,
) : AbstractNativePrimitive(Signature("context_stored_id", 2)) {
    override fun call(call: NativeCall): Int {
        val pattern = call.arg(0)
        if (call.kind(pattern) == NativeCall.VAR) return NativeCall.FAIL
        val matching = call.factsMatching(pattern, branches.selected)
        return call.attempts(
            matching
                .asSequence()
                .filter { it.record.identifier >= 0 }
                .map { stored -> { c: NativeCall -> c.unify(c.arg(1), c.int(stored.record.identifier)) } }
                .iterator(),
        )
    }
}
