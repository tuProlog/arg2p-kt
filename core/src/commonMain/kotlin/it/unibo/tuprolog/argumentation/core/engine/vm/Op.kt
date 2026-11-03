package it.unibo.tuprolog.argumentation.core.engine.vm

/**
 * The instructions of the machine (spike): a clause is compiled into a sequence of them (see [Compiler]), each an
 * opcode followed by its operands in the code array of the [Program]. The names are those of the WAM: `get` and
 * `unify` instructions unify the head against the argument registers, `put` and `set` build the arguments of a
 * body goal, the rest is control. Registers: `A` and `X` are the same array (the arguments of a call first, the
 * temporary variables of the clause after them); `Y` are the slots of the environment frame of the clause.
 */
internal object Op {
    const val GET_X_VAR = 1
    const val GET_Y_VAR = 2
    const val GET_X_VAL = 3
    const val GET_Y_VAL = 4
    const val GET_CONST = 5
    const val GET_STR = 6
    const val GET_LIST = 7
    const val UNIFY_X_VAR = 8
    const val UNIFY_Y_VAR = 9
    const val UNIFY_X_VAL = 10
    const val UNIFY_Y_VAL = 11
    const val UNIFY_CONST = 12
    const val UNIFY_VOID = 13
    const val PUT_X_VAR = 14
    const val PUT_Y_VAR = 15
    const val PUT_X_VAL = 16
    const val PUT_Y_VAL = 17
    const val PUT_CONST = 18
    const val PUT_STR = 19
    const val PUT_LIST = 20
    const val SET_X_VAR = 21
    const val SET_Y_VAR = 22
    const val SET_X_VAL = 23
    const val SET_Y_VAL = 24
    const val SET_CONST = 25
    const val SET_VOID = 26
    const val ALLOCATE = 27
    const val DEALLOCATE = 28
    const val CALL = 29
    const val EXECUTE = 30
    const val PROCEED = 31
    const val CUT = 32
    const val GET_LEVEL = 33
    const val CUT_Y = 34
    const val TRY_ELSE = 35
    const val TRUST_ME = 36
    const val JUMP = 37
    const val FAIL = 38
    const val CALL_TERM = 39
    const val EXECUTE_TERM = 40
    const val STOP = 41
    const val INIT_Y = 42
    const val RESTORE_SCOPE = 43

    /** The flag of a term call transparent to cut (the barrier of the frame) rather than opaque (its own) */
    const val TRANSPARENT = 1
    const val OPAQUE = 0
}
