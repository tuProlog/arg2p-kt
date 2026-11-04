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
    const val CUT_LOCAL = 44

    // Superinstructions: a list or a structure of two arguments with both arguments unified (the sub-instructions
    // below, three words each), a deallocation followed by the last call or by the return
    const val GET_LIST2 = 45
    const val GET_STR2 = 46
    const val DEALLOCATE_EXECUTE = 47
    const val RETURN = 48

    // Arithmetic compiled inline: `is/2` and the comparisons evaluate their expressions (postfix code of the
    // elements below, two words each) from the registers without building a term, falling back to the builtin
    // on anything but small integers
    const val ARITH_IS = 49
    const val ARITH_CMP = 50

    // A list whose head and tail are both temporaries met first: the commonest list in a head
    const val GET_LIST_VV = 51

    // Builtins compiled inline, on the variables where they are: a type test (a mask of the tags that pass, then the
    // register or the slot), == or \\== (1 to negate, then two operands: a register, or a slot as -1 - slot), the same
    // against a constant
    const val TYPE_X = 52
    const val TYPE_Y = 53
    const val IDENTICAL = 54
    const val IDENTICAL_C = 55

    // arg/3 with a constant index (the index, the term as an operand, the target kind and operand) and functor/3 (the term
    // as an operand, then the targets of the name and of the arity), anything but the common case left to the builtin
    const val ARG = 56
    const val FUNCTOR = 57

    // From here on, an instruction stands for an entry of a clause compiled (see Program.compileHot)
    const val COMPILED = 1 shl 16

    // The target of ARITH_IS: a register or a slot holding a value to unify, one to set (a first occurrence), a
    // constant
    const val T_X = 1
    const val T_Y = 2
    const val T_NEW_X = 3
    const val T_NEW_Y = 4
    const val T_CONST = 5
    const val T_VOID = 6

    // The elements of an expression
    const val E_X = 1
    const val E_Y = 2
    const val E_CONST = 3
    const val E_ADD = 4
    const val E_SUB = 5
    const val E_MUL = 6
    const val E_IDIV = 7
    const val E_MOD = 8
    const val E_NEG = 9

    // The comparisons
    const val C_LT = 1
    const val C_GT = 2
    const val C_LE = 3
    const val C_GE = 4
    const val C_EQ = 5
    const val C_NE = 6

    // The sub-instructions of GET_LIST2 and GET_STR2: the kind, then the register (or the constant) and the name
    const val U_X_VAR = 1
    const val U_Y_VAR = 2
    const val U_X_VAL = 3
    const val U_Y_VAL = 4
    const val U_CONST = 5
    const val U_VOID = 6

    /** The flag of a term call transparent to cut (the barrier of the frame) rather than opaque (its own) */
    const val TRANSPARENT = 1
    const val OPAQUE = 0
}
