package it.unibo.tuprolog.argumentation.engine.vm.program

/**
 * The instructions of the machine: a clause is compiled into a sequence of them (see [Compiler]), each an opcode
 * followed by its operands in the code array of the [Program]. The names are those of the WAM: `get` instructions
 * unify the head against the argument registers (the arguments of a structure by the sub-instructions below), `put`
 * and `set` build the arguments of a body goal, the rest is control. Registers: `A` and `X` are the same array (the arguments of a call first, the
 * temporary variables of the clause after them); `Y` are the slots of the environment frame of the clause.
 */
internal object Op {
    const val GET_X_VAR = 1
    const val GET_Y_VAR = 2
    const val GET_X_VAL = 3
    const val GET_Y_VAL = 4
    const val GET_CONST = 5
    const val PUT_X_VAR = 14
    const val PUT_Y_VAR = 15
    const val PUT_X_VAL = 16
    const val PUT_Y_VAL = 17
    const val PUT_CONST = 18
    const val PUT_STR = 19
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

    // Before the call of arg/3 with its arguments in the registers: done here, skipping the call, with an index and a compound
    const val ARG_CALL = 59

    // The cut of a clause with no frame: no goal was called before it, the barrier is still where the call left it
    const val CUT_NECK = 58

    // Pairs merged, the operands of the first and then of the second, by those executed most (see Compiler.merging)
    const val PUT_Y_VAL2 = 60
    const val PUT_Y_VAL_DEALLOCATE_EXECUTE = 61
    const val GET_Y_VAR2 = 62
    const val PUT_Y_VAR_CALL = 63
    const val GET_LIST2_EXECUTE = 64
    const val GET_LIST2_GET_LIST2 = 65
    const val GET_X_VAL_PROCEED = 66
    const val GET_CONST_PROCEED = 67

    const val GET_CONST2 = 72
    const val GET_CONST2_PROCEED = 73
    const val GET_LIST2_GET_X_VAL = 74
    const val GET_LIST2_GET_X_VAL_PROCEED = 75
    const val PUT_Y_VAL_CALL = 76

    // A structure of two arguments, or a list cell, built whole: the functor, the register, the two as sub-instructions
    const val PUT_STR2 = 77
    const val PUT_LIST2 = 78

    // The end of a call in another scope: RESTORE_SCOPE and RETURN
    const val RESTORE_SCOPE_RETURN = 68

    // Arithmetic on small integers in one instruction, ARITH_CMP or ARITH_IS following it for anything else: a comparison
    // (the kind, two operands or an operand and a value, the words of what follows), X is Y + c (the operand, c, the words
    // of what follows, whose first operands are the target)
    const val COMPARE_VV = 69
    const val COMPARE_VC = 70
    const val IS_VC = 71

    // Module::Goal with the arguments of Goal in the registers: the functor of ::/2 or :::/2, the word of the module, the
    // functor of Goal, its arity; the second as a last call (1 to deallocate first)
    const val MODULE_CALL = 79
    const val MODULE_EXECUTE = 80

    // A structure of any arity: the functor, the register, the arity, as many sub-instructions (see GET_STR2)
    const val GET_STRN = 81

    // arg/3 with a first occurrence for a slot as its value: done here, the slot set, skipping the words given (the arguments
    // put and the call, for what is left to the builtin), when it has an index and a compound (the two as operands)
    const val ARG_TO_SLOT = 82

    // GET_LIST2 with a value and a new temporary, then the last call of EXECUTE, as an append has it (the same operands)
    const val GET_LIST_VX_EXECUTE = 84

    // GET_STR2 with two new temporaries (the same operands)
    const val GET_STR_VV = 85

    // More pairs and runs merged. An opcode absorbed in the middle stays in the code, skipped over
    const val GET_CONST_CUT_PROCEED = 86
    const val CUT_CALL = 87
    const val GET_STR2_GET_STR2 = 88
    const val PUT_STR2_PUT_STR2 = 89
    const val PUT_Y_VAL3_DEALLOCATE_EXECUTE = 90
    const val PUT_Y_VAL2_CALL = 91
    const val PUT_Y_VAL2_DEALLOCATE_EXECUTE = 92
    const val ALLOCATE_GET_Y_VAR2 = 93
    const val GET_X_VAR_TYPE_X = 94

    // The return of the alternatives of a builtin behind an inline instruction (see ControlBlocks.returnBehind): to the
    // address in the first slot, the frame deallocated
    const val RETURN_BEHIND = 95

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

    /** An operation of an expression compiled inline: its functor, and its element (E_ADD...) */
    class Operation(
        val name: String,
        val arity: Int,
        val kind: Int,
    )

    /** The operations an expression compiled inline may have, the only list of them (the compiler, the arithmetic) */
    val OPERATIONS =
        listOf(
            Operation("+", 2, E_ADD),
            Operation("-", 2, E_SUB),
            Operation("*", 2, E_MUL),
            Operation("//", 2, E_IDIV),
            Operation("mod", 2, E_MOD),
            Operation("-", 1, E_NEG),
        )

    // The comparisons
    const val C_LT = 1
    const val C_GT = 2
    const val C_LE = 3
    const val C_GE = 4
    const val C_EQ = 5
    const val C_NE = 6

    /** The comparisons, by the name of their predicates */
    val COMPARISONS: Map<String, Int> =
        mapOf(
            "<" to C_LT,
            ">" to C_GT,
            "=<" to C_LE,
            ">=" to C_GE,
            "=:=" to C_EQ,
            "=\\=" to C_NE,
        )

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
