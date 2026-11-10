package it.unibo.tuprolog.argumentation.engine.vm

import it.unibo.tuprolog.argumentation.engine.natives.NativeCall.Companion.CONTINUE
import it.unibo.tuprolog.argumentation.engine.natives.NativeCall.Companion.FAIL
import it.unibo.tuprolog.argumentation.engine.natives.NativeCall.Companion.JUMPED
import it.unibo.tuprolog.argumentation.engine.natives.NativeResume
import it.unibo.tuprolog.argumentation.engine.vm.ExecutionEnvironment
import it.unibo.tuprolog.argumentation.engine.vm.program.ClauseTable
import it.unibo.tuprolog.argumentation.engine.vm.program.Op
import it.unibo.tuprolog.argumentation.engine.vm.program.Program
import it.unibo.tuprolog.argumentation.engine.vm.program.Scope
import it.unibo.tuprolog.argumentation.engine.vm.program.TermFlattener
import it.unibo.tuprolog.argumentation.engine.words.Words
import it.unibo.tuprolog.argumentation.engine.words.Words.INT
import it.unibo.tuprolog.argumentation.engine.words.Words.LIST
import it.unibo.tuprolog.argumentation.engine.words.Words.REF
import it.unibo.tuprolog.argumentation.engine.words.Words.STR
import it.unibo.tuprolog.argumentation.engine.words.payload
import it.unibo.tuprolog.argumentation.engine.words.ref
import it.unibo.tuprolog.argumentation.engine.words.shifted
import it.unibo.tuprolog.argumentation.engine.words.smallInt
import it.unibo.tuprolog.argumentation.engine.words.tag
import it.unibo.tuprolog.argumentation.engine.words.word
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.Var
import it.unibo.tuprolog.solve.exception.ResolutionException
import kotlin.jvm.JvmField

/**
 * The machine: runs the instructions the [Compiler] produces (see [Op]) over a heap of words (see [Words]),
 * a trail of the cells bound since the last choice point, a local stack of environment frames (the permanent
 * variables of the clauses, the continuation and the cut barrier of each), the choice points as records of ints,
 * and the registers. A clause is entered by jumping to its code: its head is unified against the argument
 * registers instruction by instruction, its body goals are called with their arguments put in the registers, the
 * last one in place of the clause (its frame deallocated first). Backtracking resets the tops of the heap and of
 * the trail and the registers of the machine to those a choice point saved.
 *
 * The predicates a goal is resolved with are those of the [scope] of the libraries the resolution runs with, and
 * those of the theory of the user when it is visible: a module call ([NativeCall.inline]) switches them for its
 * goal and back when the goal is done (a frame of its own holds the scope to restore). A lookup in a store
 * ([storeGoal]) resolves its goal with the records stored for it.
 *
 * An exception is a ball thrown through the code ([Raised]): the choice points are unwound to the nearest active catch/3
 * ([CATCH]) whose catcher unifies with it, and its recovery runs in its place; a ball no catch takes halts the run. A
 * catch is active while its goal runs: once the goal exits it goes, or, if the goal left choice points, it stays inactive
 * until backtracking enters the goal again ([CATCH_EXIT]).
 *
 * The heap is garbage collected ([collect]) when a call finds it above a threshold: the cells reachable from the
 * arguments of the call, from the frames reachable from the current one and from those the choice points saved,
 * from the arguments the choice points saved, from the trail and from the words pinned around a nested run are
 * kept, in their order, and the rest slides down.
 *
 * The primitives of the libraries ([PrimitiveFacade]) are called with their goal as tuProlog terms and their answers unified
 * back; a primitive with further answers leaves a choice point of its own.
 *
 * This class is the interpreter: its registers (the arguments, [pc], the continuation, the cut barrier, the frame, the scope),
 * the local stack and the choice points, which the instructions touch together at every call and backtracking (as a
 * component of their own they cost 4-6%, measured), the loop of the instructions ([execute], [run]) and the solve of a query.
 * The algorithms on that state are extension functions in files of their own (they cost nothing over members): frames and
 * choice points (MachineChoicePoints.kt), the selection of clauses (MachineClauses.kt), calls (MachineGoals.kt), the
 * instructions out of [execute] (MachineInstructions.kt), balls and catch/3 (MachineExceptions.kt), the builtins
 * (MachineBuiltins.kt). What owns state of its own is a component: the terms and the trail ([Heap]), the inline arithmetic
 * ([Arithmetic]), the words pinned across nested runs ([Pins]), the state of a native call ([NativeContext]), the nested
 * runs ([NestedRuns]), the machine as the natives see it ([NativeFace]).
 *
 * No local of [execute] lives across the calls of its arms beyond those it needs (code, ip, cpBase): a call clobbers every
 * register, and each further live value is spilled around every call (a `val heap = heap` there cost 2.5%, measured).
 */
internal class Machine(
    val program: Program,
    /** The environment the primitives of the libraries are called in, as their side effects left it */
    var environment: ExecutionEnvironment,
    // The heap cells above which a call collects (the threshold then follows the cells kept)
    @JvmField
    val gcMinimum: Int = GC_MIN,
) {
    private val pooled = MachineArrays.take()

    // The terms: the heap, the trail of the bindings
    @JvmField
    val heap = Heap(program, pooled.heap, pooled.varInfo, pooled.trail)

    // The local stack: environment frames (the previous frame, the continuation, the cut barrier, the number of
    // slots, then the slots)
    @JvmField
    var local = pooled.local

    /** The current environment frame, NONE when there is none */
    @JvmField
    var frame = NONE

    /** The continuation: the address of the instruction to go on with when the clause is done */
    @JvmField
    var continuation = 0

    /** The instruction to run next */
    var pc = 0

    /** The cut barrier of the clause being entered: the choice points a cut in it keeps */
    @JvmField
    var cutBarrier = 0

    /** The libraries the resolution runs with, and whether the theory of the user is visible */
    var scope: Scope = program.scopeOf(environment.libraries)
    var theoryVisible = true

    @JvmField
    var cps = pooled.cps

    // By choice point, those of clauses: the clauses it goes on with, and the candidates among them if an index gave them
    @JvmField internal var cpTables = arrayOfNulls<ClauseTable>(cps.size / CP_SIZE)

    @JvmField internal var cpCandidates = arrayOfNulls<IntArray>(cps.size / CP_SIZE)

    @JvmField
    var cpTop = 0

    @JvmField
    var cpArgs = pooled.cpArgs

    @JvmField
    var cpArgsTop = 0

    /** The registers: the arguments of the goal being called, then the temporary variables of the clause */
    @JvmField
    var regs = IntArray(maxOf(64, program.maxRegister + 1))

    // The words kept across nested runs
    @JvmField
    val pins = Pins()

    // The parts of the machine: the terms copied off the heap, the order of terms, the lookups of the stores, the primitives
    // that are not native, the collector of the heap
    @JvmField internal val copier = TermCopier(this)

    @JvmField
    val termOrder = TermOrder(this)

    @JvmField internal val stores = StoreLookup(this)

    @JvmField internal val primitives = PrimitiveFacade(this)
    private val collector = HeapCollector(this)

    // The builtins by functor (for each program: a machine is made for each solve)
    @JvmField internal val builtinTable = program.builtins { builtinsOf(program) }

    // The time the run must end by, and the calls made since it was last checked
    @JvmField internal var deadline = Long.MAX_VALUE

    @JvmField internal var callsSinceCheck = 0

    // Whether the theory of the user is visible when a run starts
    private var initialTheoryVisible = true

    // The calls with alternatives left that are not clauses: of primitives (their responses) and of lookups (their
    // records), referred to by the choice points
    @JvmField
    val calls = ArrayList<Any>()

    // Garbage collection
    @JvmField
    var gcThreshold = gcMinimum

    fun interface Builtin {
        /** Runs the builtin on the registers of the machine: [FAIL], [CONTINUE] at the next instruction, or [JUMPED] */
        fun call(machine: Machine): Int
    }

    // The state of the native call at hand
    @JvmField
    val natives = NativeContext()

    /** A ball thrown through the code: the term, and the exception it came from, if any */
    class Raised(
        val ball: Term,
        val exception: ResolutionException? = null,
    ) : RuntimeException() {
        // (the text of the ball made when asked: a ball a catch/3 takes is never printed)
        override val message: String get() = ball.toString()
    }

    /** A resolution halted by an exception no catch/3 took */
    class Halt(
        val exception: ResolutionException,
    ) : RuntimeException(exception.message, exception)

    // The inline arithmetic
    @JvmField internal val arithmetic = Arithmetic(program, heap)

    // The solutions of goals in nested runs: findall/3, those natives ask
    @JvmField internal val nested = NestedRuns(this)

    @JvmField
    val wComma = program.functorWord(",", 2)

    @JvmField internal val wCatchExit = program.functorWord("\$catch_exit", 1)

    // The number of the last catch/3 run, telling its choice point apart (see catchExit)
    @JvmField internal var catches = 0

    @JvmField internal val fList = program.functor(".", 2)

    // The code of the control constructs called as goals at run time, of the end of a run, of the return from an
    // inlined goal
    private val controlBlocks = program.controlBlocks()

    @JvmField internal val stopAddress = controlBlocks.stop

    @JvmField internal val restoreScopeBlock = controlBlocks.restoreScope

    @JvmField internal val returnBehindBlock = controlBlocks.returnBehind
    private val fArg = program.functor("arg", 3)
    private val fFunctor = program.functor("functor", 3)

    // An expression the machine does not evaluate itself (see builtinsOf): one object, thrown each time, its stack trace
    // (of no use: the builtin catches it) taken once
    internal object NotNative : RuntimeException()

    // The registers as many as a clause of the program uses
    fun fitRegisters() {
        if (program.maxRegister + 1 > regs.size) regs = regs.copyOf(program.maxRegister + 1)
    }

    internal fun jump(address: Int): Int {
        pc = address
        return JUMPED
    }

    // The environment as a primitive sees it where it is called: within a module call, the libraries of the module
    // and the theory of the user hidden (the solvers it creates for its sub-goals see the same), the root's otherwise;
    // kept per scope and visibility until the root changes
    private val environmentsSeen = HashMap<Int, ExecutionEnvironment>()

    fun environmentSeen(): ExecutionEnvironment {
        val base = environment
        val libraries = scope.libraries
        if ((libraries == null || libraries === base.libraries) && base.theoryVisible == theoryVisible) return base
        val key = scope.id * 2 + (if (theoryVisible) 1 else 0)
        val cached = environmentsSeen[key]
        if (cached != null && cached.root === base) return cached
        val made = base.copy(libraries = libraries ?: base.libraries, theoryVisible = theoryVisible, root = base)
        environmentsSeen[key] = made
        return made
    }

    // The machine as the natives see it (see NativeCall)
    @JvmField internal val face = NativeFace(this)

    // ---- the arrays, back to the pool

    private var released = false

    /** The arrays back for another machine: this one is done, it runs no more */
    fun release() {
        if (released) return
        released = true
        pooled.heap = heap.cells
        pooled.varInfo = heap.varInfo
        pooled.trail = heap.trail
        pooled.local = local
        pooled.cps = cps
        pooled.cpArgs = cpArgs
        MachineArrays.give(pooled)
    }

    // The choice point delayed: the clause entered has alternatives, and until it gets past its head and its guard nothing
    // else can see them, so they wait here, its arguments above the top of [cpArgs] (a head that fails, or a guard before a
    // cut, costs no choice point). -1 for none. What it does not hold is as the clause found it: the continuation and the
    // barrier, as a frame allocated meanwhile restores them
    @JvmField internal var delayedNext = -1

    @JvmField internal var delayedTable: ClauseTable? = null

    @JvmField internal var delayedCandidates: IntArray? = null

    @JvmField internal var delayedFunctor = 0

    @JvmField internal var delayedArity = 0

    @JvmField internal var delayedKey = 0

    @JvmField internal var delayedTop = 0

    @JvmField internal var delayedTrail = 0

    @JvmField internal var delayedFrame = NONE

    // Whether the arguments are saved: a clause that only tests (see ClauseCode.guarded) leaves them where they are
    @JvmField internal var delayedSaved = false

    // Whether the arguments of the goal beyond the first are all unbound
    @JvmField internal var delayedFree = false

    // ---- the instructions

    /**
     * Runs the code from [pc] until the continuation of the run reaches its end (true) or an instruction fails
     * (false); the program counter lives in a local meanwhile, written to [pc] around the calls
     */
    fun execute(cpBase: Int): Boolean {
        var code = program.code
        var ip = pc
        while (true) {
            dispatch@ while (true) {
                val at = ip
                when (code[at]) {
                    Op.GET_X_VAR -> {
                        regs[code[at + 1]] = regs[code[at + 2]]
                        ip = at + 3
                    }
                    Op.GET_Y_VAR -> {
                        local[frame + FRAME + code[at + 1]] = regs[code[at + 2]]
                        ip = at + 3
                    }
                    Op.GET_X_VAL -> {
                        if (!heap.unify(regs[code[at + 1]], regs[code[at + 2]])) break@dispatch
                        ip = at + 3
                    }
                    Op.GET_Y_VAL -> {
                        if (!heap.unify(local[frame + FRAME + code[at + 1]], regs[code[at + 2]])) break@dispatch
                        ip = at + 3
                    }
                    Op.GET_CONST -> {
                        if (!heap.unifyConstant(code[at + 1], regs[code[at + 2]])) break@dispatch
                        ip = at + 3
                    }
                    Op.GET_LIST_VV -> {
                        val d = heap.dereferenced(regs[code[at + 1]])
                        when (tag(d)) {
                            REF -> writeListOfVariables(code, at, d)
                            LIST -> {
                                val a = payload(d)
                                regs[code[at + 2]] = heap.cells[a]
                                regs[code[at + 4]] = heap.cells[a + 1]
                            }
                            else -> break@dispatch
                        }
                        ip = at + 6
                    }
                    Op.GET_LIST2 -> {
                        if (!getList2(code, at)) break@dispatch
                        ip = at + 8
                    }
                    Op.GET_LIST2_GET_LIST2 -> {
                        if (!getList2(code, at) || !getList2(code, at + 7)) break@dispatch
                        ip = at + 15
                    }
                    Op.GET_LIST2_EXECUTE -> {
                        if (!getList2(code, at)) break@dispatch
                        ip = goal(code[at + 8], code[at + 9], -1)
                        if (ip < 0) break@dispatch
                        code = program.code
                    }
                    Op.GET_X_VAL_PROCEED -> {
                        if (!heap.unify(regs[code[at + 1]], regs[code[at + 2]])) break@dispatch
                        ip = proceeded()
                    }
                    Op.GET_CONST_PROCEED -> {
                        if (!heap.unifyConstant(code[at + 1], regs[code[at + 2]])) break@dispatch
                        ip = proceeded()
                    }
                    Op.GET_CONST2 -> {
                        if (!heap.unifyConstant(code[at + 1], regs[code[at + 2]]) ||
                            !heap.unifyConstant(code[at + 3], regs[code[at + 4]])
                        ) {
                            break@dispatch
                        }
                        ip = at + 5
                    }
                    Op.GET_CONST2_PROCEED -> {
                        if (!heap.unifyConstant(code[at + 1], regs[code[at + 2]]) ||
                            !heap.unifyConstant(code[at + 3], regs[code[at + 4]])
                        ) {
                            break@dispatch
                        }
                        ip = proceeded()
                    }
                    Op.GET_LIST2_GET_X_VAL -> {
                        if (!getList2(code, at) || !heap.unify(regs[code[at + 8]], regs[code[at + 9]])) break@dispatch
                        ip = at + 10
                    }
                    Op.GET_LIST2_GET_X_VAL_PROCEED -> {
                        if (!getList2(code, at) || !heap.unify(regs[code[at + 8]], regs[code[at + 9]])) break@dispatch
                        ip = proceeded()
                    }
                    Op.PUT_Y_VAL_CALL -> {
                        regs[code[at + 2]] = local[frame + FRAME + code[at + 1]]
                        ip = goal(code[at + 3], code[at + 4], at + 5)
                        if (ip < 0) break@dispatch
                        code = program.code
                    }
                    Op.MODULE_CALL -> {
                        settleHere()
                        ip = at + 5
                        continuation = ip
                        cutBarrier = cpTop
                        pc = ip
                        when (moduleCall(code[at + 1], code[at + 2], code[at + 3], code[at + 4])) {
                            FAIL -> break@dispatch
                            JUMPED -> ip = pc
                            else -> Unit
                        }
                        code = program.code
                    }
                    Op.MODULE_EXECUTE -> {
                        if (code[at + 5] != 0) deallocate()
                        settleHere()
                        cutBarrier = cpTop
                        when (moduleCall(code[at + 1], code[at + 2], code[at + 3], code[at + 4])) {
                            FAIL -> break@dispatch
                            CONTINUE -> ip = continuation
                            else -> ip = pc
                        }
                        code = program.code
                    }
                    Op.PUT_STR -> {
                        val h = heap.alloc(1)
                        heap.cells[h] = code[at + 1]
                        regs[code[at + 2]] = word(STR, h)
                        ip = at + 3
                    }
                    Op.PUT_STR2 -> {
                        putStructure2(code, at)
                        ip = at + 9
                    }
                    Op.PUT_LIST2 -> {
                        putList2(code, at)
                        ip = at + 8
                    }
                    Op.GET_Y_VAR2 -> {
                        val slots = frame + FRAME
                        local[slots + code[at + 1]] = regs[code[at + 2]]
                        local[slots + code[at + 3]] = regs[code[at + 4]]
                        ip = at + 5
                    }
                    Op.PUT_Y_VAL2 -> {
                        val slots = frame + FRAME
                        regs[code[at + 2]] = local[slots + code[at + 1]]
                        regs[code[at + 4]] = local[slots + code[at + 3]]
                        ip = at + 5
                    }
                    Op.PUT_Y_VAL_DEALLOCATE_EXECUTE -> {
                        regs[code[at + 2]] = local[frame + FRAME + code[at + 1]]
                        deallocate()
                        ip = goal(code[at + 3], code[at + 4], -1)
                        if (ip < 0) break@dispatch
                        code = program.code
                    }
                    Op.PUT_Y_VAR_CALL -> {
                        settleHere()
                        val h = heap.newVariable(code[at + 3])
                        local[frame + FRAME + code[at + 1]] = ref(h)
                        regs[code[at + 2]] = ref(h)
                        ip = goal(code[at + 4], code[at + 5], at + 6)
                        if (ip < 0) break@dispatch
                        code = program.code
                    }
                    Op.RESTORE_SCOPE_RETURN -> {
                        scope = program.scope(payload(local[frame + FRAME]))
                        theoryVisible = payload(local[frame + FRAME + 1]) != 0
                        deallocate()
                        ip = proceeded()
                    }
                    Op.GET_CONST_CUT_PROCEED -> {
                        if (!heap.unifyConstant(code[at + 1], regs[code[at + 2]])) break@dispatch
                        cutNeck()
                        ip = continuation
                    }
                    Op.CUT_CALL -> {
                        cutTo(local[frame + FRAME_BARRIER])
                        ip = goal(code[at + 1], code[at + 2], at + 3)
                        if (ip < 0) break@dispatch
                        code = program.code
                    }
                    Op.GET_STR2_GET_STR2 -> {
                        if (!getStructure2(code, at) || !getStructure2(code, at + 9)) break@dispatch
                        ip = at + 18
                    }
                    Op.PUT_STR2_PUT_STR2 -> {
                        putStructure2(code, at)
                        putStructure2(code, at + 9)
                        ip = at + 18
                    }
                    Op.PUT_Y_VAL3_DEALLOCATE_EXECUTE -> {
                        val slots = frame + FRAME
                        regs[code[at + 2]] = local[slots + code[at + 1]]
                        regs[code[at + 4]] = local[slots + code[at + 3]]
                        regs[code[at + 7]] = local[slots + code[at + 6]]
                        deallocate()
                        ip = goal(code[at + 8], code[at + 9], -1)
                        if (ip < 0) break@dispatch
                        code = program.code
                    }
                    Op.PUT_Y_VAL2_CALL -> {
                        val slots = frame + FRAME
                        regs[code[at + 2]] = local[slots + code[at + 1]]
                        regs[code[at + 4]] = local[slots + code[at + 3]]
                        ip = goal(code[at + 5], code[at + 6], at + 7)
                        if (ip < 0) break@dispatch
                        code = program.code
                    }
                    Op.PUT_Y_VAL2_DEALLOCATE_EXECUTE -> {
                        val slots = frame + FRAME
                        regs[code[at + 2]] = local[slots + code[at + 1]]
                        regs[code[at + 4]] = local[slots + code[at + 3]]
                        deallocate()
                        ip = goal(code[at + 5], code[at + 6], -1)
                        if (ip < 0) break@dispatch
                        code = program.code
                    }
                    Op.ALLOCATE_GET_Y_VAR2 -> {
                        allocate(code[at + 1])
                        val slots = frame + FRAME
                        local[slots + code[at + 3]] = regs[code[at + 4]]
                        local[slots + code[at + 5]] = regs[code[at + 6]]
                        ip = at + 7
                    }
                    Op.GET_X_VAR_TYPE_X -> {
                        regs[code[at + 1]] = regs[code[at + 2]]
                        if ((code[at + 3] ushr heap.typeTag(heap.dereferenced(regs[code[at + 4]]))) and 1 == 0) break@dispatch
                        ip = at + 5
                    }
                    Op.GET_STR2 -> {
                        if (!getStructure2(code, at)) break@dispatch
                        ip = at + 9
                    }
                    Op.GET_STRN -> {
                        if (!getStructureN(code, at)) break@dispatch
                        ip = at + 4 + code[at + 3] * 3
                    }
                    Op.ARG_TO_SLOT -> {
                        val index = heap.dereferenced(operand(code[at + 1]))
                        if (tag(index) == INT && payload(index) >= 0) {
                            val answer = inlineArg(payload(index), operand(code[at + 2]), Op.T_NEW_Y, code[at + 3])
                            if (answer == FAIL) break@dispatch
                            ip = if (answer == CONTINUE) at + 5 + code[at + 4] else at + 5
                        } else {
                            ip = at + 5
                        }
                    }
                    Op.GET_STR_VV -> {
                        val d = heap.dereferenced(regs[code[at + 2]])
                        when (tag(d)) {
                            STR -> {
                                val a = payload(d)
                                if (heap.cells[a] != code[at + 1]) break@dispatch
                                regs[code[at + 4]] = heap.cells[a + 1]
                                regs[code[at + 7]] = heap.cells[a + 2]
                            }
                            REF -> writeStructureOfVariables(code, at, d)
                            else -> break@dispatch
                        }
                        ip = at + 9
                    }
                    Op.GET_LIST_VX_EXECUTE -> {
                        val d = heap.dereferenced(regs[code[at + 1]])
                        when (tag(d)) {
                            REF -> {
                                val h = heap.alloc(2)
                                heap.bind(payload(d), word(LIST, h))
                                heap.cells[h] = regs[code[at + 3]]
                                heap.cells[h + 1] = ref(h + 1)
                                heap.varInfo[h + 1] = code[at + 7]
                                regs[code[at + 6]] = ref(h + 1)
                            }
                            LIST -> {
                                val a = payload(d)
                                if (!heap.unify(regs[code[at + 3]], heap.cells[a])) break@dispatch
                                regs[code[at + 6]] = heap.cells[a + 1]
                            }
                            else -> break@dispatch
                        }
                        ip = goal(code[at + 8], code[at + 9], -1)
                        if (ip < 0) break@dispatch
                        code = program.code
                    }
                    Op.PUT_X_VAR -> {
                        val h = heap.newVariable(code[at + 3])
                        regs[code[at + 1]] = ref(h)
                        regs[code[at + 2]] = ref(h)
                        ip = at + 4
                    }
                    Op.PUT_Y_VAR -> {
                        val h = heap.newVariable(code[at + 3])
                        local[frame + FRAME + code[at + 1]] = ref(h)
                        regs[code[at + 2]] = ref(h)
                        ip = at + 4
                    }
                    Op.PUT_X_VAL -> {
                        regs[code[at + 2]] = regs[code[at + 1]]
                        ip = at + 3
                    }
                    Op.PUT_Y_VAL -> {
                        regs[code[at + 2]] = local[frame + FRAME + code[at + 1]]
                        ip = at + 3
                    }
                    Op.PUT_CONST -> {
                        regs[code[at + 2]] = code[at + 1]
                        ip = at + 3
                    }
                    Op.SET_X_VAR -> {
                        regs[code[at + 1]] = ref(heap.newVariable(code[at + 2]))
                        ip = at + 3
                    }
                    Op.SET_Y_VAR -> {
                        local[frame + FRAME + code[at + 1]] = ref(heap.newVariable(code[at + 2]))
                        ip = at + 3
                    }
                    Op.SET_X_VAL -> {
                        heap.pushCell(regs[code[at + 1]])
                        ip = at + 2
                    }
                    Op.SET_Y_VAL -> {
                        heap.pushCell(local[frame + FRAME + code[at + 1]])
                        ip = at + 2
                    }
                    Op.SET_CONST -> {
                        heap.pushCell(code[at + 1])
                        ip = at + 2
                    }
                    Op.SET_VOID -> {
                        heap.pushVoids(code[at + 1])
                        ip = at + 2
                    }
                    Op.ALLOCATE -> {
                        allocate(code[at + 1])
                        ip = at + 2
                    }
                    Op.DEALLOCATE -> {
                        deallocate()
                        ip = at + 1
                    }
                    Op.CALL -> {
                        ip = goal(code[at + 1], code[at + 2], at + 3)
                        if (ip < 0) break@dispatch
                        code = program.code
                    }
                    Op.EXECUTE -> {
                        ip = goal(code[at + 1], code[at + 2], -1)
                        if (ip < 0) break@dispatch
                        code = program.code
                    }
                    Op.DEALLOCATE_EXECUTE -> {
                        deallocate()
                        ip = goal(code[at + 1], code[at + 2], -1)
                        if (ip < 0) break@dispatch
                        code = program.code
                    }
                    Op.RETURN -> {
                        deallocate()
                        ip = proceeded()
                    }
                    Op.RETURN_BEHIND -> ip = returnedBehind()
                    Op.PROCEED -> {
                        ip = proceeded()
                    }
                    Op.CUT_NECK -> {
                        cutNeck()
                        ip = at + 1
                    }
                    Op.CUT -> {
                        cutTo(local[frame + FRAME_BARRIER])
                        ip = at + 1
                    }
                    Op.GET_LEVEL -> {
                        settleHere()
                        local[frame + FRAME + code[at + 1]] = smallInt(cpTop)
                        ip = at + 2
                    }
                    Op.CUT_Y -> {
                        cutTo(payload(local[frame + FRAME + code[at + 1]]))
                        ip = at + 2
                    }
                    Op.CUT_LOCAL -> {
                        settleHere()
                        cutTo(payload(local[frame + FRAME + code[at + 1]]) + 1)
                        ip = at + 2
                    }
                    Op.TRY_ELSE -> {
                        delayAlternative(code[at + 1])
                        ip = at + 2
                    }
                    Op.TRUST_ME -> ip = at + 1
                    Op.JUMP -> ip = code[at + 1]
                    Op.FAIL -> break@dispatch
                    Op.CALL_TERM -> {
                        settleHere()
                        val goal = local[frame + FRAME + code[at + 1]]
                        ip = at + 3
                        continuation = ip
                        cutBarrier = if (code[at + 2] == Op.TRANSPARENT) local[frame + FRAME_BARRIER] else cpTop
                        pc = ip
                        when (callTerm(goal)) {
                            FAIL -> break@dispatch
                            JUMPED -> ip = pc
                            else -> Unit
                        }
                        code = program.code
                    }
                    Op.EXECUTE_TERM -> {
                        settleHere()
                        val goal = regs[code[at + 1]]
                        if (code[at + 2] == Op.OPAQUE) cutBarrier = cpTop
                        when (callTerm(goal)) {
                            FAIL -> break@dispatch
                            CONTINUE -> ip = continuation
                            else -> ip = pc
                        }
                        code = program.code
                    }
                    Op.STOP -> {
                        pc = at
                        return true
                    }
                    Op.COMPARE_VV -> {
                        val a = heap.dereferenced(operand(code[at + 2]))
                        val b = heap.dereferenced(operand(code[at + 3]))
                        if (tag(a) == INT && tag(b) == INT) {
                            if (!holds(code[at + 1], payload(a), payload(b))) break@dispatch
                            ip = at + 5 + code[at + 4]
                        } else {
                            ip = at + 5
                        }
                    }
                    Op.COMPARE_VC -> {
                        val a = heap.dereferenced(operand(code[at + 2]))
                        if (tag(a) == INT) {
                            if (!holds(code[at + 1], payload(a), code[at + 3])) break@dispatch
                            ip = at + 5 + code[at + 4]
                        } else {
                            ip = at + 5
                        }
                    }
                    Op.IS_VC -> {
                        val a = heap.dereferenced(operand(code[at + 1]))
                        if (tag(a) == INT) {
                            val w = heap.sumWord(payload(a), code[at + 2])
                            val operand = code[at + 6]
                            when (val kind = code[at + 5]) {
                                Op.T_NEW_X -> regs[operand] = w
                                Op.T_NEW_Y -> local[frame + FRAME + operand] = w
                                else -> if (!toTarget(kind, operand, w)) break@dispatch
                            }
                            ip = at + 4 + code[at + 3]
                        } else {
                            ip = at + 4
                        }
                    }
                    Op.ARITH_IS -> {
                        val n = code[at + 3]
                        ip = at + 4 + n
                        val w = arithmetic.word(code, at + 4, n, regs, local, frame + FRAME)
                        if (w != 0) {
                            val operand = code[at + 2]
                            when (code[at + 1]) {
                                Op.T_X -> if (!heap.unify(regs[operand], w)) break@dispatch
                                Op.T_Y -> if (!heap.unify(local[frame + FRAME + operand], w)) break@dispatch
                                Op.T_NEW_X -> regs[operand] = w
                                Op.T_NEW_Y -> local[frame + FRAME + operand] = w
                                else -> if (operand != w) break@dispatch
                            }
                        } else {
                            // The builtin: the target and the expression as words in the registers, which are kept
                            // (the expression first: a target new to the clause may take the register of an operand)
                            val expression = arithmetic.build(code, at + 4, n, regs, local, frame + FRAME)
                            ip = builtinBehind(arithmetic.fIs, targetWord(code[at + 1], code[at + 2]), expression, ip)
                            if (ip < 0) break@dispatch
                            code = program.code
                        }
                    }
                    Op.ARITH_CMP -> {
                        val nA = code[at + 2]
                        val nB = code[at + 3 + nA]
                        ip = at + 4 + nA + nB
                        val answer = arithmetic.comparison(code, at, nA, nB, regs, local, frame + FRAME)
                        if (answer == 0) break@dispatch
                        if (answer < 0) {
                            settleHere()
                            val left = arithmetic.build(code, at + 3, nA, regs, local, frame + FRAME)
                            ip =
                                builtinBehind(
                                    arithmetic.comparisonFunctors[code[at + 1]],
                                    left,
                                    arithmetic.build(
                                        code,
                                        at + 4 + nA,
                                        nB,
                                        regs,
                                        local,
                                        frame + FRAME,
                                    ),
                                    ip,
                                )
                            if (ip < 0) break@dispatch
                            code = program.code
                        }
                    }
                    Op.TYPE_X -> {
                        if ((code[at + 1] ushr heap.typeTag(heap.dereferenced(regs[code[at + 2]]))) and 1 == 0) break@dispatch
                        ip = at + 3
                    }
                    Op.TYPE_Y -> {
                        if ((code[at + 1] ushr heap.typeTag(heap.deref(local[frame + FRAME + code[at + 2]]))) and 1 == 0) break@dispatch
                        ip = at + 3
                    }
                    Op.IDENTICAL -> {
                        if (HeapTerms.identical(this, operand(code[at + 2]), operand(code[at + 3])) ==
                            (code[at + 1] != 0)
                        ) {
                            break@dispatch
                        }
                        ip = at + 4
                    }
                    Op.IDENTICAL_C -> {
                        if ((heap.deref(operand(code[at + 2])) == code[at + 3]) == (code[at + 1] != 0)) break@dispatch
                        ip = at + 4
                    }
                    Op.ARG -> {
                        ip = at + 5
                        when (inlineArg(code[at + 1], operand(code[at + 2]), code[at + 3], code[at + 4])) {
                            FAIL -> break@dispatch
                            CONTINUE -> Unit
                            else -> {
                                val term = operand(code[at + 2])
                                val target = targetWord(code[at + 3], code[at + 4])
                                ip = builtinBehind3(fArg, program.smallWord(code[at + 1]), term, target, ip)
                                if (ip < 0) break@dispatch
                                code = program.code
                            }
                        }
                    }
                    Op.ARG_CALL -> {
                        val index = heap.deref(regs[0])
                        val answer =
                            if (tag(index) == INT &&
                                payload(index) >= 0
                            ) {
                                inlineArg(payload(index), regs[1], Op.T_X, 2)
                            } else {
                                NOT_INLINE
                            }
                        when (answer) {
                            FAIL -> break@dispatch
                            CONTINUE ->
                                when (code[at + 1]) {
                                    Op.CALL -> ip = at + 4
                                    Op.EXECUTE -> {
                                        settleHere()
                                        ip = continuation
                                    }
                                    else -> {
                                        deallocate()
                                        settleHere()
                                        ip = continuation
                                    }
                                }
                            else -> ip = at + 1
                        }
                    }
                    Op.FUNCTOR -> {
                        ip = at + 6
                        when (inlineFunctor(operand(code[at + 1]), code[at + 2], code[at + 3], code[at + 4], code[at + 5])) {
                            FAIL -> break@dispatch
                            CONTINUE -> Unit
                            else -> {
                                val term = operand(code[at + 1])
                                val name = targetWord(code[at + 2], code[at + 3])
                                ip = builtinBehind3(fFunctor, term, name, targetWord(code[at + 4], code[at + 5]), ip)
                                if (ip < 0) break@dispatch
                                code = program.code
                            }
                        }
                    }
                    Op.INIT_Y -> {
                        local[frame + FRAME + code[at + 1]] = ref(heap.newVariable(code[at + 2]))
                        ip = at + 3
                    }
                    else -> throw IllegalStateException("unknown instruction ${code[at]} at $at")
                }
            }
            // A failure: the alternatives of clauses and conditions here, the rest (a primitive may raise) for [run]
            if (!backtrackHere(cpBase)) return false
            ip = pc
            code = program.code
        }
    }

    /**
     * Runs above the choice point [cpBase] until a run reaches its end (true: a solution) or no choice point above [cpBase]
     * is left (false). It starts by running the code at [pc] ([EXECUTE]), by backtracking for another solution
     * ([BACKTRACK]), or by calling the goal [goal] ([CALL]). A ball any step raises (the code, a builtin, the resumption of a
     * primitive by backtracking, a recovery) is unwound to the nearest catch/3 above [cpBase] that takes it, whose recovery
     * is the next step; a ball none takes is thrown on
     */
    fun run(
        cpBase: Int,
        start: Int = EXECUTE,
        goal: Int = NONE,
    ): Boolean {
        var step = start
        var called = goal
        while (true) {
            try {
                when (step) {
                    BACKTRACK -> if (!backtrack(cpBase)) return false
                    CALL ->
                        when (callTerm(called)) {
                            FAIL -> {
                                step = BACKTRACK
                                continue
                            }
                            CONTINUE -> pc = continuation
                            else -> Unit
                        }
                }
                if (execute(cpBase)) return true
                step = BACKTRACK
            } catch (raised: Raised) {
                called = unwind(raised, cpBase)
                if (called == NONE) throw raised
                step = CALL
            }
        }
    }

    // ---- solutions and records (the nested runs: NestedRuns)

    /** A copy of the term [w0] on the heap, its unbound variables fresh and unnamed */
    fun copyOnHeap(w0: Int): Int {
        val bagBase = copier.top
        val root = copier.copy(w0)
        val shift = copier.toHeap(bagBase, named = false)
        copier.top = bagBase
        return shifted(root, shift)
    }

    // The key at hand, of no use past the lookup
    @JvmField internal val keys = TermFlattener(program, 256)

    // ---- native primitives

    // What the code of a native primitive does: any exception of it a ball
    internal inline fun guardingNative(call: () -> Int): Int =
        try {
            call()
        } catch (raised: Raised) {
            throw raised
        } catch (halt: Halt) {
            throw halt
        } catch (exception: Exception) {
            throw raised(exception)
        }

    @JvmField internal var moduleGoalFunctor = 0

    @JvmField internal var moduleGoalArity = 0

    // ---- the collector

    /** Collects the heap cells no longer reachable, [arity] registers holding the arguments of the call at hand */
    fun collect(arity: Int) = collector.collect(arity)

    /** How many times the heap was collected */
    val collections: Int get() = collector.collections

    // ---- the solve of a query

    /** The machine emptied: a top-level query starts from nothing */
    private fun reset() {
        heap.reset()
        frame = NONE
        delayedNext = -1
        cpTop = 0
        cpArgsTop = 0
        pins.truncate(0)
        calls.clear()
        theoryVisible = initialTheoryVisible
        callsSinceCheck = 0
        scope = program.scopeOf(environment.libraries)
    }

    /**
     * The solutions of [query]: each the bindings of its variables, until none is left; a ball no catch/3 takes
     * ends them with a [Halt]
     */
    fun solve(
        query: Struct,
        deadline: Long = Long.MAX_VALUE,
        theoryVisible: Boolean = true,
    ): Sequence<Map<Var, Term>> =
        sequence {
            this@Machine.deadline = deadline
            initialTheoryVisible = theoryVisible
            reset()
            val compiled = program.compileQuery(query)
            fitRegisters()
            // (the cells of the variables of the query, as the arguments of its code, pinned: the collector moves them)
            val queryPins = LinkedHashMap<Var, Int>()
            val cells = compiled.variables.size
            heap.queryCells = cells
            if (heap.queryBound.size < cells) heap.queryBound = IntArray(cells)
            heap.queryBound.fill(0, 0, cells)
            heap.bindings = 0
            for ((i, variable) in compiled.variables.withIndex()) {
                val cell = heap.queryVariable(variable)
                regs[i] = ref(cell)
                queryPins[variable] = pins.pin(ref(cell))
            }
            continuation = stopAddress
            cutBarrier = 0
            pc = compiled.start
            var found = runToSolution(first = true)
            while (found) {
                val names = HashMap<Int, Var>()
                // (the variables left unbound are not bindings; the others in the order they were bound, as the
                // engine reported them)
                yield(
                    queryPins.entries
                        .filter { (variable, pin) ->
                            !variable.isAnonymous && heap.cells[payload(pins.words[pin])].let { it != pins.words[pin] }
                        }.sortedBy { (_, pin) -> heap.queryBound[pin] }
                        .associate { (variable, pin) -> variable to heap.toTerm(pins.words[pin], names) },
                )
                found = runToSolution(first = false)
            }
        }

    private fun runToSolution(first: Boolean): Boolean =
        try {
            run(0, if (first) EXECUTE else BACKTRACK)
        } catch (raised: Raised) {
            throw Halt(haltOf(raised))
        }

    companion object {
        const val NONE = -1

        // A frame: the previous frame at its address, then the continuation, the cut barrier and the number of slots; the
        // slots follow from FRAME on
        const val FRAME_CONTINUATION = 1
        const val FRAME_BARRIER = 2
        const val FRAME_SLOTS = 3
        const val FRAME = 4

        // A choice point: the mark of the trail at its address, then the tops of the heap and of the local stack, the frame,
        // the continuation, the instruction to resume at, its kind (the functor of its clauses, or one of those below)...
        const val CP_HEAP_TOP = 1
        const val CP_LOCAL_TOP = 2
        const val CP_FRAME = 3
        const val CP_CONTINUATION = 4
        const val CP_RESUME_AT = 5
        const val CP_KIND = 6

        // ... the next of its clauses (or the index of its call among those left), the cut barrier of its clauses, where its
        // arguments are saved, how many calls were left, how many arguments it has, the key of its goal (for a lookup, the
        // position of the next record)...
        const val CP_NEXT = 7
        const val CP_BARRIER = 8
        const val CP_ARGS = 9
        const val CP_CALLS = 10
        const val CP_ARITY = 11
        const val CP_KEY = 12

        // ... and the scope, whether the theory of the user is visible, the top of the pinned words, where its clauses come from
        const val CP_SCOPE = 13
        const val CP_THEORY_VISIBLE = 14
        const val CP_PINNED = 15
        const val CP_SOURCE = 16
        const val CP_SIZE = 17

        // Those of a catch/3: its number, and whether it is inactive (see catchExit)
        const val CP_CATCH_NUMBER = CP_KEY
        const val CP_CATCH_INACTIVE = CP_NEXT

        // The functors of the choice points that are not clause alternatives
        const val BARRIER = -2
        const val RESUME = -3
        const val PRIMITIVE = -4
        const val CATCH = -5
        const val STORED = -6
        const val NATIVE = -7

        // Above the choice points a catch/3 goal left when it exited: backtracking to it makes the catch active again
        const val CATCH_EXIT = -8

        // Where the clauses of a choice point come from: the libraries and the theory, or the theory alone
        internal const val MERGED = 0
        internal const val THEORY = 1

        // (as [MERGED], the arguments of the goal beyond the first all unbound)
        internal const val MERGED_FREE = 2

        // An inline builtin leaving the call to the builtin itself
        internal const val NOT_INLINE = StdlibNatives.NOT_NATIVE

        // How a run starts (see run): the code at pc, backtracking, the call of a goal
        const val EXECUTE = 0
        const val BACKTRACK = 1
        const val CALL = 2

        // The heap cells above which a call collects
        const val GC_MIN = 1 shl 20

        // The calls between two looks at the clock
        internal const val CHECK_EVERY = 1024

        /** The message of the halt of a resolution its caller interrupted */
        const val INTERRUPTED = "The resolution was interrupted by its caller"
    }
}

// The alternatives of a native call, with the variables its terms gave the cells (the same on resumption, for
// the cells still there)
internal class NativeAlternatives(
    val resume: NativeResume,
    val names: HashMap<Int, Var>?,
)
