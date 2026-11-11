package it.unibo.tuprolog.argumentation.engine.vm

import it.unibo.tuprolog.argumentation.engine.natives.NativeCall.Companion.CONTINUE
import it.unibo.tuprolog.argumentation.engine.natives.NativeCall.Companion.FAIL
import it.unibo.tuprolog.argumentation.engine.natives.NativeCall.Companion.JUMPED
import it.unibo.tuprolog.argumentation.engine.vm.Machine.Companion.FRAME
import it.unibo.tuprolog.argumentation.engine.vm.Machine.Companion.FRAME_SLOTS
import it.unibo.tuprolog.argumentation.engine.vm.Machine.Companion.NOT_INLINE
import it.unibo.tuprolog.argumentation.engine.vm.program.Op
import it.unibo.tuprolog.argumentation.engine.words.Words.ATOM
import it.unibo.tuprolog.argumentation.engine.words.Words.FUN
import it.unibo.tuprolog.argumentation.engine.words.Words.INT
import it.unibo.tuprolog.argumentation.engine.words.Words.LIST
import it.unibo.tuprolog.argumentation.engine.words.Words.REF
import it.unibo.tuprolog.argumentation.engine.words.Words.STR
import it.unibo.tuprolog.argumentation.engine.words.payload
import it.unibo.tuprolog.argumentation.engine.words.ref
import it.unibo.tuprolog.argumentation.engine.words.smallInt
import it.unibo.tuprolog.argumentation.engine.words.tag
import it.unibo.tuprolog.argumentation.engine.words.word
// What the instructions do out of execute, to keep it under the 8000 bytes the JIT compiles: the specialised instructions
// on structures and lists (GET_LIST2, GET_STR2, GET_STRN, PUT_STR2, PUT_LIST2) and their sub-instructions on a cell, the
// inline builtins (arg/3, functor/3, the targets and operands of their arguments), the builtin behind an inline
// instruction that left it a case (its alternatives in the theory returning through RETURN_BEHIND); the expression of an
// inline arithmetic as a term is Arithmetic.build

// A builtin in place of what an instruction did not do inline, its arguments put in the first registers, which are kept: the
// address to go on at, -1 if it failed
internal fun Machine.builtinBehind(
    functor: Int,
    first: Int,
    second: Int,
    next: Int,
): Int {
    val a0 = regs[0]
    val a1 = regs[1]
    regs[0] = first
    regs[1] = second
    return behind(functor, 2, next, a0, a1, 0)
}

internal fun Machine.builtinBehind3(
    functor: Int,
    first: Int,
    second: Int,
    third: Int,
    next: Int,
): Int {
    val a0 = regs[0]
    val a1 = regs[1]
    val a2 = regs[2]
    regs[0] = first
    regs[1] = second
    regs[2] = third
    return behind(functor, 3, next, a0, a1, a2)
}

// The builtin of [functor] on its [arity] arguments in the registers, the clause going on at [next] (the address returned,
// -1 if it failed) with the registers it held, [a0], [a1], [a2], back. The continuation and the cut barrier of the clause
// stay, as the clause may have no frame to find them in
private fun Machine.behind(
    functor: Int,
    arity: Int,
    next: Int,
    a0: Int,
    a1: Int,
    a2: Int,
): Int {
    settle()
    // (a builtin with clauses, of the theory or of a library: its alternatives return through a frame of their own)
    if (predicateOf(functor) != null) return behindWithAlternatives(functor, arity, next, a0, a1, a2)
    val savedContinuation = continuation
    val savedBarrier = cutBarrier
    continuation = next
    cutBarrier = cpTop
    pc = next
    return when (callUncollected(functor, arity)) {
        FAIL -> -1
        JUMPED -> pc
        else -> {
            continuation = savedContinuation
            cutBarrier = savedBarrier
            regs[0] = a0
            regs[1] = a1
            if (arity == 3) regs[2] = a2
            next
        }
    }
}

// [behind] of a builtin the theory of the user gives clauses, its alternatives: they may run later, by backtracking, when
// the clause has gone on. So they return through a frame of their own (see ControlBlocks.returnBehind), which allocate
// makes holding the continuation and the cut barrier of the clause, its slots [next] and the registers
private fun Machine.behindWithAlternatives(
    functor: Int,
    arity: Int,
    next: Int,
    a0: Int,
    a1: Int,
    a2: Int,
): Int {
    // (all the registers the code may use: an alternative may run any clause before the clause goes on with its temporaries;
    // the collector reads the slots as roots, what is stale there it skips)
    val held = maxOf(program.maxRegister + 1, arity)
    allocate(1 + held)
    val slots = frame + FRAME
    local[slots] = smallInt(next)
    copyWords(regs, 0, local, slots + 1, held)
    local[slots + 1] = a0
    local[slots + 2] = a1
    if (arity == 3) local[slots + 3] = a2
    continuation = returnBehindBlock
    cutBarrier = cpTop
    pc = next
    return when (callUncollected(functor, arity)) {
        FAIL -> -1
        JUMPED -> pc
        // (done at once: as an alternative returning, the frame gone; a choice point keeps it for the alternatives)
        else -> returnedBehind()
    }
}

// RETURN_BEHIND: the registers the clause held back from the frame, the frame deallocated, the address after the
// instruction
internal fun Machine.returnedBehind(): Int {
    val slots = frame + FRAME
    val held = local[frame + FRAME_SLOTS] - 1
    for (i in 0 until held) regs[i] = local[slots + 1 + i]
    val next = payload(local[slots])
    deallocate()
    settle()
    return next
}

// The builtin of [functor] called, with no collection: the temporaries of the clause are live, and they are no roots
private fun Machine.callUncollected(
    functor: Int,
    arity: Int,
): Int {
    val threshold = gcThreshold
    gcThreshold = Int.MAX_VALUE
    return try {
        call(functor, arity)
    } finally {
        gcThreshold = threshold
    }
}

// GET_STR_VV at [at] against the variable [d]
internal fun Machine.writeStructureOfVariables(
    code: IntArray,
    at: Int,
    d: Int,
) {
    val h = heap.alloc(3)
    heap.bind(payload(d), word(STR, h))
    heap.cells[h] = code[at + 1]
    heap.cells[h + 1] = ref(h + 1)
    heap.varInfo[h + 1] = code[at + 5]
    heap.cells[h + 2] = ref(h + 2)
    heap.varInfo[h + 2] = code[at + 8]
    regs[code[at + 4]] = ref(h + 1)
    regs[code[at + 7]] = ref(h + 2)
}

// GET_LIST_VV at [at] against the variable [d]
internal fun Machine.writeListOfVariables(
    code: IntArray,
    at: Int,
    d: Int,
) {
    val h = heap.alloc(2)
    heap.cells[h] = ref(h)
    heap.varInfo[h] = code[at + 3]
    heap.cells[h + 1] = ref(h + 1)
    heap.varInfo[h + 1] = code[at + 5]
    regs[code[at + 2]] = ref(h)
    regs[code[at + 4]] = ref(h + 1)
    heap.bind(payload(d), word(LIST, h))
}

// The word of an operand of an inline builtin: a register, or a slot as -1 - slot
internal fun Machine.operand(o: Int): Int = if (o >= 0) regs[o] else local[frame + FRAME - 1 - o]

// The target of an inline builtin as a word (a first occurrence created where it lives)
internal fun Machine.targetWord(
    kind: Int,
    operand: Int,
): Int =
    when (kind) {
        Op.T_X -> regs[operand]
        Op.T_Y -> local[frame + FRAME + operand]
        Op.T_NEW_X -> ref(heap.newVariable()).also { regs[operand] = it }
        Op.T_NEW_Y -> ref(heap.newVariable()).also { local[frame + FRAME + operand] = it }
        Op.T_VOID -> ref(heap.newVariable())
        else -> operand
    }

// The word [w] as the value of a target
internal fun Machine.toTarget(
    kind: Int,
    operand: Int,
    w: Int,
): Boolean =
    when (kind) {
        Op.T_X -> heap.unify(regs[operand], w)
        Op.T_Y -> heap.unify(local[frame + FRAME + operand], w)
        Op.T_NEW_X -> {
            regs[operand] = w
            true
        }
        Op.T_NEW_Y -> {
            local[frame + FRAME + operand] = w
            true
        }
        Op.T_VOID -> true
        else -> heap.unify(operand, w)
    }

// arg/3 with the index [i]: [CONTINUE] or [FAIL], [NOT_INLINE] for what is left to the builtin (what is not compound)
internal fun Machine.inlineArg(
    i: Int,
    term0: Int,
    kind: Int,
    operand: Int,
): Int {
    val term = heap.dereferenced(term0)
    val w =
        when (heap.typeTag(term)) {
            STR -> {
                val a = payload(term)
                if (i < 1 || i > program.arity(heap.cells[a])) return FAIL
                heap.cells[a + i]
            }
            LIST -> {
                if (i < 1 || i > 2) return FAIL
                heap.cells[payload(term) + i - 1]
            }
            else -> return NOT_INLINE
        }
    return if (toTarget(kind, operand, w)) CONTINUE else FAIL
}

// functor/3 of a term that is bound: [CONTINUE] or [FAIL], [NOT_INLINE] for what is left to the builtin (a term to build, an arity that is not one)
internal fun Machine.inlineFunctor(
    term0: Int,
    nameKind: Int,
    nameOperand: Int,
    arityKind: Int,
    arityOperand: Int,
): Int {
    val term = heap.dereferenced(term0)
    val t = tag(term)
    if (t == REF) return inlineFunctorBuilding(term, nameKind, nameOperand, arityKind, arityOperand)
    if (arityKind == Op.T_X || arityKind == Op.T_Y || arityKind == Op.T_CONST) {
        val arity = heap.dereferenced(targetWord(arityKind, arityOperand))
        if (tag(arity) != REF && (tag(arity) != INT || payload(arity) < 0)) return NOT_INLINE
    }
    val name: Int
    val n: Int
    when (if (t == STR) heap.typeTag(term) else t) {
        STR -> {
            val f = heap.cells[payload(term)]
            name = word(ATOM, program.symbols.functorNames[payload(f)])
            n = program.arity(f)
        }
        LIST -> {
            name = program.dotWord
            n = 2
        }
        else -> {
            name = term
            n = 0
        }
    }
    return if (toTarget(nameKind, nameOperand, name) &&
        toTarget(arityKind, arityOperand, program.smallWord(n))
    ) {
        CONTINUE
    } else {
        FAIL
    }
}

// functor/3 with the variable [term]: the compound of a name and of a positive arity built, the rest left to the builtin
internal fun Machine.inlineFunctorBuilding(
    term: Int,
    nameKind: Int,
    nameOperand: Int,
    arityKind: Int,
    arityOperand: Int,
): Int {
    if (nameKind == Op.T_NEW_X || nameKind == Op.T_NEW_Y || nameKind == Op.T_VOID) return NOT_INLINE
    if (arityKind == Op.T_NEW_X || arityKind == Op.T_NEW_Y || arityKind == Op.T_VOID) return NOT_INLINE
    val name = heap.dereferenced(targetWord(nameKind, nameOperand))
    val arity = heap.dereferenced(targetWord(arityKind, arityOperand))
    if (tag(name) != ATOM || tag(arity) != INT || payload(arity) <= 0) return NOT_INLINE
    val n = payload(arity)
    val built: Int
    if (name == program.dotWord) {
        if (n != 2) return NOT_INLINE
        built = word(LIST, heap.top)
        heap.pushVoids(2)
    } else {
        val h = heap.alloc(1)
        heap.cells[h] = word(FUN, program.functor(payload(name), n))
        heap.pushVoids(n)
        built = word(STR, h)
    }
    heap.bind(payload(term), built)
    return CONTINUE
}

/** arg/3 with the index [i], its arguments in the registers: [CONTINUE] or [FAIL], negative for what is left to the library */
internal fun Machine.argOnRegisters(i: Int): Int = inlineArg(i, regs[1], Op.T_X, 2)

/** functor/3 with its arguments in the registers: [CONTINUE] or [FAIL], negative for what is left to the library */
internal fun Machine.functorOnRegisters(): Int = inlineFunctor(regs[0], Op.T_X, 1, Op.T_X, 2)

// The instruction GET_LIST2 at [at] of [code]
internal fun Machine.getList2(
    code: IntArray,
    at: Int,
): Boolean {
    val d = heap.dereferenced(regs[code[at + 1]])
    when (tag(d)) {
        LIST -> {
            val a = payload(d)
            return readCell(code[at + 2], code[at + 3], a) && readCell(code[at + 5], code[at + 6], a + 1)
        }
        REF -> {
            val h = heap.alloc(2)
            heap.bind(payload(d), word(LIST, h))
            writeCell(code[at + 2], code[at + 3], code[at + 4], h)
            writeCell(code[at + 5], code[at + 6], code[at + 7], h + 1)
            return true
        }
        else -> return false
    }
}

// The instruction GET_STR2 at [at] of [code]
internal fun Machine.getStructure2(
    code: IntArray,
    at: Int,
): Boolean {
    val f = code[at + 1]
    val d = heap.dereferenced(regs[code[at + 2]])
    when (tag(d)) {
        STR -> {
            val a = payload(d)
            if (heap.cells[a] != f) return false
            return readCell(code[at + 3], code[at + 4], a + 1) && readCell(code[at + 6], code[at + 7], a + 2)
        }
        REF -> {
            val h = heap.alloc(3)
            heap.cells[h] = f
            heap.bind(payload(d), word(STR, h))
            writeCell(code[at + 3], code[at + 4], code[at + 5], h + 1)
            writeCell(code[at + 6], code[at + 7], code[at + 8], h + 2)
            return true
        }
        else -> return false
    }
}

// The instruction GET_STRN at [at] of [code]
internal fun Machine.getStructureN(
    code: IntArray,
    at: Int,
): Boolean {
    val f = code[at + 1]
    val n = code[at + 3]
    val d = heap.dereferenced(regs[code[at + 2]])
    var sub = at + 4
    when (tag(d)) {
        STR -> {
            val a = payload(d)
            if (heap.cells[a] != f) return false
            for (i in 1..n) {
                if (!readCell(code[sub], code[sub + 1], a + i)) return false
                sub += 3
            }
            return true
        }
        REF -> {
            val h = heap.alloc(n + 1)
            heap.cells[h] = f
            heap.bind(payload(d), word(STR, h))
            for (i in 1..n) {
                writeCell(code[sub], code[sub + 1], code[sub + 2], h + i)
                sub += 3
            }
            return true
        }
        else -> return false
    }
}

// The instruction PUT_STR2 at [at] of [code]
internal fun Machine.putStructure2(
    code: IntArray,
    at: Int,
) {
    val h = heap.alloc(3)
    heap.cells[h] = code[at + 1]
    regs[code[at + 2]] = word(STR, h)
    writeCell(code[at + 3], code[at + 4], code[at + 5], h + 1)
    writeCell(code[at + 6], code[at + 7], code[at + 8], h + 2)
}

// The instruction PUT_LIST2 at [at] of [code]
internal fun Machine.putList2(
    code: IntArray,
    at: Int,
) {
    val h = heap.alloc(2)
    regs[code[at + 1]] = word(LIST, h)
    writeCell(code[at + 2], code[at + 3], code[at + 4], h)
    writeCell(code[at + 5], code[at + 6], code[at + 7], h + 1)
}

// A sub-instruction (see Op) on the argument of a structure at [address]
@Suppress("NOTHING_TO_INLINE")
internal inline fun Machine.readCell(
    kind: Int,
    a: Int,
    address: Int,
): Boolean {
    when (kind) {
        Op.U_X_VAR -> regs[a] = heap.cells[address]
        Op.U_Y_VAR -> local[frame + FRAME + a] = heap.cells[address]
        Op.U_X_VAL -> return heap.unify(regs[a], heap.cells[address])
        Op.U_Y_VAL -> return heap.unify(local[frame + FRAME + a], heap.cells[address])
        Op.U_CONST -> {
            val w = heap.dereferenced(heap.cells[address])
            if (tag(w) == REF) {
                heap.bind(payload(w), a)
            } else if (w != a) {
                return false
            }
        }
        else -> Unit
    }
    return true
}

// A sub-instruction (see Op) building the argument of a structure in the cell [cell], already reserved
@Suppress("NOTHING_TO_INLINE")
internal inline fun Machine.writeCell(
    kind: Int,
    a: Int,
    b: Int,
    cell: Int,
) {
    when (kind) {
        Op.U_X_VAR -> {
            heap.cells[cell] = ref(cell)
            heap.varInfo[cell] = b
            regs[a] = ref(cell)
        }
        Op.U_Y_VAR -> {
            heap.cells[cell] = ref(cell)
            heap.varInfo[cell] = b
            local[frame + FRAME + a] = ref(cell)
        }
        Op.U_X_VAL -> heap.cells[cell] = regs[a]
        Op.U_Y_VAL -> heap.cells[cell] = local[frame + FRAME + a]
        Op.U_CONST -> heap.cells[cell] = a
        else -> {
            heap.cells[cell] = ref(cell)
            heap.varInfo[cell] = 0
        }
    }
}
