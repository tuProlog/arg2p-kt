package it.unibo.tuprolog.argumentation.engine.vm

/**
 * The instructions of a clause into the code of a [Program], those that may merge with the one before them merged in place
 * as they are emitted (the opcode of the first rewritten, the operands of the second following its own; see [Op]). A label
 * placed, or anything else emitted in between, ends what may merge.
 */
internal class CodeEmitter(
    private val program: Program,
) {
    /** Whether the clause being compiled has a frame of its own */
    var needsEnv = false

    // The last instruction that may merge with the next: where it is, and where it ends (anything emitted since, or a
    // label there, and it may not)
    private var mergingAt = -1
    private var mergingEnd = -1

    fun merging(at: Int) {
        mergingAt = at
        mergingEnd = program.codeTop
    }

    fun lastIs(opcode: Int): Boolean = mergingEnd == program.codeTop && program.code[mergingAt] == opcode

    // The last instruction becomes [merged], the operands of the next following its own
    fun mergeInto(merged: Int) {
        program.code[mergingAt] = merged
        mergingEnd = -1
    }

    fun placeLabel(label: Int) {
        program.code[label] = program.codeTop
        mergingEnd = -1
        pairEnd = -1
    }

    // The last PUT_Y_VAL2, where it is and where it ends; the ALLOCATE opening the clause; the GET_CONST before a neck cut
    private var pairAt = -1
    private var pairEnd = -1
    private var allocateAt = -1
    private var allocateEnd = -1
    private var constBeforeCut = -1

    fun emitPutYVal(
        slot: Int,
        register: Int,
    ) {
        if (lastIs(Op.PUT_Y_VAL)) {
            pairAt = mergingAt
            mergeOn(Op.PUT_Y_VAL2, slot, register)
            pairEnd = program.codeTop
        } else {
            val at = program.codeTop
            program.emit(Op.PUT_Y_VAL, slot, register)
            merging(at)
        }
    }

    fun emitGetYVar(
        slot: Int,
        register: Int,
    ) {
        if (lastIs(Op.GET_Y_VAR)) {
            if (mergingAt == allocateEnd) {
                program.code[allocateAt] = Op.ALLOCATE_GET_Y_VAR2
                mergingEnd = -1
            } else {
                mergeInto(Op.GET_Y_VAR2)
            }
            program.emit(slot)
            program.emit(register)
        } else {
            val at = program.codeTop
            program.emit(Op.GET_Y_VAR, slot, register)
            merging(at)
        }
    }

    fun emitMerging(
        opcode: Int,
        a: Int,
        b: Int,
    ) {
        val at = program.codeTop
        program.emit(opcode, a, b)
        merging(at)
    }

    fun emitGetConst(
        constant: Int,
        register: Int,
    ) {
        if (lastIs(Op.GET_CONST)) {
            mergeOn(Op.GET_CONST2, constant, register)
        } else {
            emitMerging(Op.GET_CONST, constant, register)
        }
    }

    fun emitGetXVal(
        a: Int,
        b: Int,
    ) {
        if (lastIs(Op.GET_LIST2)) {
            mergeOn(Op.GET_LIST2_GET_X_VAL, a, b)
        } else {
            emitMerging(Op.GET_X_VAL, a, b)
        }
    }

    // The last instruction becomes [merged], with the operands [a] and [b], and may merge further
    private fun mergeOn(
        merged: Int,
        a: Int,
        b: Int,
    ) {
        val at = mergingAt
        mergeInto(merged)
        program.emit(a)
        program.emit(b)
        merging(at)
    }

    // The call of a goal, last or not, with what it may merge with
    fun emitCall(
        last: Boolean,
        functor: Int,
        arity: Int,
    ) {
        when {
            !last && lastIs(Op.PUT_Y_VAR) -> {
                mergeInto(Op.PUT_Y_VAR_CALL)
                program.emit(functor)
                program.emit(arity)
            }
            !last && lastIs(Op.PUT_Y_VAL) -> {
                mergeInto(Op.PUT_Y_VAL_CALL)
                program.emit(functor)
                program.emit(arity)
            }
            !last && lastIs(Op.PUT_Y_VAL2) -> {
                mergeInto(Op.PUT_Y_VAL2_CALL)
                program.emit(functor)
                program.emit(arity)
            }
            !last && lastIs(Op.CUT) -> {
                mergeInto(Op.CUT_CALL)
                program.emit(functor)
                program.emit(arity)
            }
            !last -> program.emit(Op.CALL, functor, arity)
            needsEnv && lastIs(Op.PUT_Y_VAL2) -> {
                mergeInto(Op.PUT_Y_VAL2_DEALLOCATE_EXECUTE)
                program.emit(functor)
                program.emit(arity)
            }
            needsEnv && lastIs(Op.PUT_Y_VAL) && mergingAt == pairEnd -> {
                program.code[pairAt] = Op.PUT_Y_VAL3_DEALLOCATE_EXECUTE
                mergingEnd = -1
                program.emit(functor)
                program.emit(arity)
            }
            needsEnv && lastIs(Op.PUT_Y_VAL) -> {
                mergeInto(Op.PUT_Y_VAL_DEALLOCATE_EXECUTE)
                program.emit(functor)
                program.emit(arity)
            }
            needsEnv -> program.emit(Op.DEALLOCATE_EXECUTE, functor, arity)
            lastIs(Op.GET_LIST2) -> {
                val appending = program.code[mergingAt + 2] == Op.U_X_VAL && program.code[mergingAt + 5] == Op.U_X_VAR
                mergeInto(if (appending) Op.GET_LIST_VX_EXECUTE else Op.GET_LIST2_EXECUTE)
                program.emit(functor)
                program.emit(arity)
            }
            else -> program.emit(Op.EXECUTE, functor, arity)
        }
    }

    fun emitProceed() {
        when {
            needsEnv -> program.emit(Op.RETURN)
            lastIs(Op.CUT_NECK) && constBeforeCut >= 0 -> {
                program.code[constBeforeCut] = Op.GET_CONST_CUT_PROCEED
                mergingEnd = -1
            }
            lastIs(Op.GET_X_VAL) -> mergeInto(Op.GET_X_VAL_PROCEED)
            lastIs(Op.GET_CONST) -> mergeInto(Op.GET_CONST_PROCEED)
            lastIs(Op.GET_CONST2) -> mergeInto(Op.GET_CONST2_PROCEED)
            lastIs(Op.GET_LIST2_GET_X_VAL) -> mergeInto(Op.GET_LIST2_GET_X_VAL_PROCEED)
            else -> program.emit(Op.PROCEED)
        }
    }

    /** The start of the code of a clause: the frame of [slots] slots it has, if it has one */
    fun openClause(slots: Int) {
        allocateEnd = -1
        if (needsEnv) {
            allocateAt = program.codeTop
            program.emit(Op.ALLOCATE, slots)
            allocateEnd = program.codeTop
        }
    }

    /** The cut of the clause: of its frame if it has one, otherwise a neck cut */
    fun emitCut() {
        if (!needsEnv) constBeforeCut = if (lastIs(Op.GET_CONST)) mergingAt else -1
        val at = program.codeTop
        program.emit(if (needsEnv) Op.CUT else Op.CUT_NECK)
        merging(at)
    }

    /** The last instruction becomes [merged], its operands kept and those of what comes next following, and merges no further */
    fun rewriteLast(merged: Int) {
        program.code[mergingAt] = merged
        mergingEnd = -1
    }

    /** What comes next does not merge with what came before */
    fun breakMerging() {
        mergingEnd = -1
    }
}
