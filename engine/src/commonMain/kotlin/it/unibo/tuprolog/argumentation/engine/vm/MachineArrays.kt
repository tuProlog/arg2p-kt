package it.unibo.tuprolog.argumentation.engine.vm

import it.unibo.tuprolog.argumentation.engine.words.locked

/**
 * The large arrays of a machine, kept for the next: a solve makes a machine, and most need the same room. What they hold is
 * garbage to a new machine, as what lies above the top of a stack is
 */
internal class MachineArrays private constructor() {
    var heap = IntArray(1 shl 16)
    var varInfo = IntArray(1 shl 16)
    var trail = IntArray(1 shl 12)
    var local = IntArray(1 shl 12)
    var cps = IntArray(Machine.CP_SIZE shl 10)
    var cpArgs = IntArray(1 shl 12)

    companion object {
        private const val KEPT = 8

        // (a solve that grew this much is rare, and what it took should go)
        private const val LARGEST_HEAP = 1 shl 21
        private val free = ArrayList<MachineArrays>()

        fun take(): MachineArrays =
            locked(free) {
                if (free.isEmpty()) MachineArrays() else free.removeAt(free.size - 1)
            }

        fun give(arrays: MachineArrays) {
            if (arrays.heap.size > LARGEST_HEAP) return
            locked(free) {
                if (free.size < KEPT) free.add(arrays)
            }
        }
    }
}
