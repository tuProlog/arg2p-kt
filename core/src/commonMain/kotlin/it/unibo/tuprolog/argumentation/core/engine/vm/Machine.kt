package it.unibo.tuprolog.argumentation.core.engine.vm

import it.unibo.tuprolog.argumentation.core.engine.ExecutionEnvironment
import it.unibo.tuprolog.argumentation.core.engine.PrologExecutionContext
import it.unibo.tuprolog.argumentation.core.engine.vm.Words.ATOM
import it.unibo.tuprolog.argumentation.core.engine.vm.Words.BIG
import it.unibo.tuprolog.argumentation.core.engine.vm.Words.FLT
import it.unibo.tuprolog.argumentation.core.engine.vm.Words.FUN
import it.unibo.tuprolog.argumentation.core.engine.vm.Words.INT
import it.unibo.tuprolog.argumentation.core.engine.vm.Words.LIST
import it.unibo.tuprolog.argumentation.core.engine.vm.Words.REF
import it.unibo.tuprolog.argumentation.core.engine.vm.Words.STR
import it.unibo.tuprolog.argumentation.core.engine.vm.Words.payload
import it.unibo.tuprolog.argumentation.core.engine.vm.Words.ref
import it.unibo.tuprolog.argumentation.core.engine.vm.Words.tag
import it.unibo.tuprolog.argumentation.core.engine.vm.Words.word
import it.unibo.tuprolog.core.Atom
import it.unibo.tuprolog.core.Cons
import it.unibo.tuprolog.core.Integer
import it.unibo.tuprolog.core.Real
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.Var
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.Solution
import it.unibo.tuprolog.solve.exception.ResolutionException
import it.unibo.tuprolog.solve.primitive.Primitive
import it.unibo.tuprolog.solve.primitive.Solve
import it.unibo.tuprolog.solve.sideffects.SideEffect
import it.unibo.tuprolog.core.List as LogicList

/**
 * The machine (spike): runs the instructions the [Compiler] produces (see [Op]) over a heap of words (see [Words]),
 * a trail of the cells bound since the last choice point, a local stack of environment frames (the permanent
 * variables of the clauses, the continuation and the cut barrier of each), the choice points as records of ints,
 * and the registers. A clause is entered by jumping to its code: its head is unified against the argument
 * registers instruction by instruction, its body goals are called with their arguments put in the registers, the
 * last one in place of the clause (its frame deallocated first). Backtracking resets the tops of the heap and of
 * the trail and the registers of the machine to those a choice point saved.
 *
 * The heap is garbage collected ([collect]) when a call finds it above a threshold: the cells reachable from the
 * arguments of the call, from the frames reachable from the current one and from those the choice points saved,
 * from the arguments the choice points saved, from the trail and from the words pinned around a nested run are
 * kept, in their order, and the rest slides down.
 *
 * The primitives of the libraries ([facade]) are called with their goal as tuProlog terms and their answers unified
 * back; a primitive with further answers leaves a choice point of its own.
 */
internal class Machine(
    val program: Program,
    // The heap cells above which a call collects (the threshold then follows the cells kept)
    private val gcMinimum: Int = GC_MIN,
) {
    var heap = IntArray(1 shl 16)
    var top = 0

    // The cells below this address are trailed when bound (the heap top at the latest choice point)
    private var heapBarrier = 0
    private var trail = IntArray(1 shl 12)
    private var trailTop = 0

    // The local stack: environment frames (the previous frame, the continuation, the cut barrier, the number of
    // slots, then the slots)
    private var local = IntArray(1 shl 12)

    /** The current environment frame, NONE when there is none */
    var frame = NONE

    /** The continuation: the address of the instruction to go on with when the clause is done */
    var continuation = 0

    /** The instruction to run next */
    var pc = 0

    /** The cut barrier of the clause being entered: the choice points a cut in it keeps */
    var cutBarrier = 0

    // Structure unification: the next cell to read, or writing
    private var next = 0
    private var writing = false

    private var cps = IntArray(CP_SIZE shl 10)
    var cpTop = 0
        private set
    private var cpArgs = IntArray(1 shl 12)
    private var cpArgsTop = 0

    /** The registers: the arguments of the goal being called, then the temporary variables of the clause */
    var regs = IntArray(64)

    private var pdl = IntArray(1 shl 10)

    // The words the code around a nested run holds: relocated by the collector
    private var pinnedWords = IntArray(16)
    private var pinnedWordsTop = 0

    // Set by the STOP instruction: the continuation of a run reached its end
    private var stopped = false

    private val builtins = HashMap<Int, Builtin>()
    private var builtinTable = arrayOfNulls<Builtin>(0)

    // The facade: the environment the primitives of the libraries are called in, their implementations by functor
    private var environment: ExecutionEnvironment? = null
    private var primitiveTable = arrayOfNulls<Primitive>(0)
    private var primitiveLooked = BooleanArray(0)
    private val primitiveCalls = ArrayList<PrimitiveCall>()

    // Garbage collection
    private var gcThreshold = gcMinimum
    private var forward = IntArray(0)
    private var markStack = IntArray(1 shl 10)
    private var frameVisited = BooleanArray(0)

    /** How many times the heap was collected, and how many cells it held before and after the last collection */
    var collections = 0
        private set
    var collectedFrom = 0
        private set
    var collectedTo = 0
        private set

    fun interface Builtin {
        /** Runs the builtin on the registers of the machine: [FAILED], [CONTINUE] at the next instruction, or [JUMPED] */
        fun call(machine: Machine): Int
    }

    // A call of a primitive with answers left: its responses, and the cells of the variables of its goal
    private class PrimitiveCall(
        val responses: Iterator<Solve.Response>,
        val variables: Array<Var>,
        val cells: IntArray,
    )

    /** A resolution halted by the exception of a primitive */
    class Halt(
        val exception: ResolutionException,
    ) : RuntimeException(exception.message, exception)

    // Arithmetic functors
    private val fPlus = program.functor("+", 2)
    private val fMinus = program.functor("-", 2)
    private val fTimes = program.functor("*", 2)
    private val fIntDiv = program.functor("//", 2)
    private val fMod = program.functor("mod", 2)
    private val fNeg = program.functor("-", 1)

    private val fIfThen = program.functorWord("->", 2)
    private val fList = program.functor(".", 2)
    private val failGoal = program.atomWord("fail")

    // The code of the control constructs called as goals at run time, and of the end of a run
    private val compiler = Compiler(program)
    private val stopAddress = compiler.stopBlock()
    private val conjunctionBlock = compiler.conjunctionBlock()
    private val disjunctionBlock = compiler.disjunctionBlock()
    private val ifThenElseBlock = compiler.ifThenElseBlock()
    private val negationBlock = compiler.negationBlock()

    init {
        builtin("true", 0) { CONTINUE }
        builtin("fail", 0) { FAILED }
        builtin("false", 0) { FAILED }
        builtin("!", 0) { m ->
            m.cutTo(m.cutBarrier)
            CONTINUE
        }
        builtin("=", 2) { m -> if (m.unify(m.regs[0], m.regs[1])) CONTINUE else FAILED }
        builtin("is", 2) { m -> if (m.unify(m.regs[0], m.program.intWord(m.eval(m.regs[1])))) CONTINUE else FAILED }
        builtin("<", 2) { m -> if (m.eval(m.regs[0]) < m.eval(m.regs[1])) CONTINUE else FAILED }
        builtin(">", 2) { m -> if (m.eval(m.regs[0]) > m.eval(m.regs[1])) CONTINUE else FAILED }
        builtin("=<", 2) { m -> if (m.eval(m.regs[0]) <= m.eval(m.regs[1])) CONTINUE else FAILED }
        builtin(">=", 2) { m -> if (m.eval(m.regs[0]) >= m.eval(m.regs[1])) CONTINUE else FAILED }
        builtin("=:=", 2) { m -> if (m.eval(m.regs[0]) == m.eval(m.regs[1])) CONTINUE else FAILED }
        builtin("=\\=", 2) { m -> if (m.eval(m.regs[0]) != m.eval(m.regs[1])) CONTINUE else FAILED }
        builtin("var", 1) { m -> if (tag(m.deref(m.regs[0])) == REF) CONTINUE else FAILED }
        builtin("nonvar", 1) { m -> if (tag(m.deref(m.regs[0])) != REF) CONTINUE else FAILED }
        builtin("call", 1) { m ->
            m.cutBarrier = m.cpTop
            m.callTerm(m.regs[0])
        }
        builtin(",", 2) { m -> m.jump(conjunctionBlock) }
        builtin(";", 2) { m ->
            val left = m.deref(m.regs[0])
            if (tag(left) == STR && m.heap[payload(left)] == fIfThen) {
                val a = payload(left)
                m.regs[2] = m.regs[1]
                m.regs[0] = m.heap[a + 1]
                m.regs[1] = m.heap[a + 2]
                m.jump(ifThenElseBlock)
            } else {
                m.jump(disjunctionBlock)
            }
        }
        builtin("->", 2) { m ->
            m.regs[2] = failGoal
            m.jump(ifThenElseBlock)
        }
        builtin("\\+", 1) { m -> m.jump(negationBlock) }
        builtin("findall", 3) { m -> m.findall(m.regs[0], m.regs[1], m.regs[2]) }
    }

    fun builtin(
        name: String,
        arity: Int,
        implementation: Builtin,
    ) {
        builtins[program.functor(name, arity)] = implementation
    }

    /** Prepares the dispatch tables: to call once the program is loaded */
    fun prepare() {
        val functors = program.functorNames.size
        builtinTable = arrayOfNulls(functors)
        for ((functor, builtin) in builtins) builtinTable[functor] = builtin
        if (program.maxRegister + 1 > regs.size) regs = regs.copyOf(program.maxRegister + 1)
    }

    /** The primitives of the libraries of [environment] answer the goals the program and the builtins do not */
    fun facade(environment: ExecutionEnvironment) {
        this.environment = environment
        primitiveTable = arrayOfNulls(0)
        primitiveLooked = BooleanArray(0)
        // (the clauses of the libraries, as any program clause: the builtins still answer the goals they do)
        for (library in environment.libraries.libraries) program.add(library.clauses)
        prepare()
    }

    private fun jump(address: Int): Int {
        pc = address
        return JUMPED
    }

    // ---- heap

    fun alloc(cells: Int): Int {
        if (top + cells > heap.size) heap = heap.copyOf(maxOf(heap.size * 2, top + cells))
        val start = top
        top += cells
        return start
    }

    fun newVariable(): Int {
        val a = alloc(1)
        heap[a] = ref(a)
        return a
    }

    fun deref(w0: Int): Int {
        var w = w0
        while (tag(w) == REF) {
            val c = heap[payload(w)]
            if (c == w) return w
            w = c
        }
        return w
    }

    private fun bind(
        address: Int,
        w: Int,
    ) {
        heap[address] = w
        if (address < heapBarrier) {
            if (trailTop == trail.size) trail = trail.copyOf(trail.size * 2)
            trail[trailTop++] = address
        }
    }

    private fun undoTrail(mark: Int) {
        while (trailTop > mark) {
            val a = trail[--trailTop]
            heap[a] = ref(a)
        }
    }

    // ---- unification

    fun unify(
        x0: Int,
        y0: Int,
    ): Boolean {
        var sp = 0
        pdl[sp++] = x0
        pdl[sp++] = y0
        while (sp > 0) {
            val y = deref(pdl[--sp])
            val x = deref(pdl[--sp])
            if (x == y) continue
            val tx = tag(x)
            val ty = tag(y)
            if (tx == REF) {
                if (ty == REF) bindVariables(payload(x), payload(y)) else bind(payload(x), y)
                continue
            }
            if (ty == REF) {
                bind(payload(y), x)
                continue
            }
            if (tx != ty) return false
            when (tx) {
                STR -> {
                    val ax = payload(x)
                    val ay = payload(y)
                    if (heap[ax] != heap[ay]) return false
                    val n = program.arity(heap[ax])
                    if (sp + 2 * n > pdl.size) pdl = pdl.copyOf(maxOf(pdl.size * 2, sp + 2 * n))
                    for (i in 1..n) {
                        pdl[sp++] = heap[ax + i]
                        pdl[sp++] = heap[ay + i]
                    }
                }
                LIST -> {
                    val ax = payload(x)
                    val ay = payload(y)
                    if (sp + 4 > pdl.size) pdl = pdl.copyOf(pdl.size * 2)
                    pdl[sp++] = heap[ax + 1]
                    pdl[sp++] = heap[ay + 1]
                    pdl[sp++] = heap[ax]
                    pdl[sp++] = heap[ay]
                }
                else -> return false
            }
        }
        return true
    }

    // The younger variable bound to the older
    private fun bindVariables(
        a: Int,
        b: Int,
    ) {
        if (a < b) bind(b, ref(a)) else bind(a, ref(b))
    }

    // A constant against a word: bound to it if a variable, the same word otherwise
    private fun unifyConstant(
        constant: Int,
        w: Int,
    ): Boolean {
        val d = deref(w)
        if (tag(d) == REF) {
            bind(payload(d), constant)
            return true
        }
        return d == constant
    }

    // ---- frames and choice points

    private fun frameEnd(e: Int): Int = if (e == NONE) 0 else e + FRAME + local[e + 3]

    private fun localMark(): Int = if (cpTop == 0) 0 else cps[(cpTop - 1) * CP_SIZE + 2]

    private fun allocate(slots: Int) {
        val e = maxOf(frameEnd(frame), localMark())
        if (e + FRAME + slots > local.size) local = local.copyOf(maxOf(local.size * 2, e + FRAME + slots))
        local[e] = frame
        local[e + 1] = continuation
        local[e + 2] = cutBarrier
        local[e + 3] = slots
        for (i in 0 until slots) local[e + FRAME + i] = UNSET
        frame = e
    }

    private fun deallocate() {
        continuation = local[frame + 1]
        cutBarrier = local[frame + 2]
        frame = local[frame]
    }

    private fun pushChoicePoint(
        functor: Int,
        nextClause: Int,
        arity: Int,
        barrier: Int,
        resumeAt: Int,
    ) {
        val base = cpTop * CP_SIZE
        if (base + CP_SIZE > cps.size) cps = cps.copyOf(cps.size * 2)
        cps[base] = trailTop
        cps[base + 1] = top
        cps[base + 2] = maxOf(frameEnd(frame), localMark())
        cps[base + 3] = frame
        cps[base + 4] = continuation
        cps[base + 5] = resumeAt
        cps[base + 6] = functor
        cps[base + 7] = nextClause
        cps[base + 8] = barrier
        cps[base + 9] = cpArgsTop
        cps[base + 10] = primitiveCalls.size
        cps[base + 11] = arity
        if (cpArgsTop + arity > cpArgs.size) cpArgs = cpArgs.copyOf(maxOf(cpArgs.size * 2, cpArgsTop + arity))
        regs.copyInto(cpArgs, cpArgsTop, 0, arity)
        cpArgsTop += arity
        cpTop++
        heapBarrier = top
    }

    // Restores the state saved by the top choice point (which stays)
    private fun restoreTop() {
        val base = (cpTop - 1) * CP_SIZE
        undoTrail(cps[base])
        top = cps[base + 1]
        frame = cps[base + 3]
        continuation = cps[base + 4]
        pc = cps[base + 5]
    }

    private fun popChoicePoint() {
        cpTop--
        val base = cpTop * CP_SIZE
        cpArgsTop = cps[base + 9]
        truncatePrimitiveCalls(cps[base + 10])
        heapBarrier = if (cpTop > 0) cps[(cpTop - 1) * CP_SIZE + 1] else 0
    }

    private fun truncatePrimitiveCalls(size: Int) {
        while (primitiveCalls.size > size) primitiveCalls.removeAt(primitiveCalls.size - 1)
    }

    fun cutTo(barrier: Int) {
        if (cpTop > barrier) {
            val base = barrier * CP_SIZE
            cpArgsTop = cps[base + 9]
            truncatePrimitiveCalls(cps[base + 10])
            cpTop = barrier
            heapBarrier = if (cpTop > 0) cps[(cpTop - 1) * CP_SIZE + 1] else 0
        }
    }

    /**
     * Resumes the latest choice point above [cpBase] that leads somewhere: false when none is left (the machine
     * then restored to the state of the oldest one popped, if any)
     */
    fun backtrack(cpBase: Int): Boolean {
        while (cpTop > cpBase) {
            restoreTop()
            val base = (cpTop - 1) * CP_SIZE
            val functor = cps[base + 6]
            val nextClause = cps[base + 7]
            val barrier = cps[base + 8]
            when (functor) {
                BARRIER -> {
                    popChoicePoint()
                    return false
                }
                RESUME -> {
                    popChoicePoint()
                    return true
                }
                PRIMITIVE -> {
                    val call = primitiveCalls[nextClause]
                    val response = call.responses.next()
                    if (!call.responses.hasNext()) popChoicePoint()
                    if (applyResponse(response, call)) return true
                }
                else -> {
                    val argsBase = cps[base + 9]
                    val arity = cps[base + 11]
                    cpArgs.copyInto(regs, 0, argsBase, argsBase + arity)
                    popChoicePoint()
                    if (tryClauses(program.predicateTable[functor]!!, nextClause, arity, barrier)) return true
                }
            }
        }
        return false
    }

    // ---- calls

    private fun keyOfHeap(w: Int): Int {
        val d = deref(w)
        return when (tag(d)) {
            REF -> 0
            STR -> heap[payload(d)]
            LIST -> Program.LIST_KEY
            else -> d
        }
    }

    // Enters the first clause of [predicate] from [from] whose key matches the first argument, leaving a choice
    // point if another may; false when none matches
    private fun tryClauses(
        predicate: Program.Predicate,
        from: Int,
        arity: Int,
        barrier: Int,
    ): Boolean {
        val clauses = predicate.clauses
        val size = clauses.size
        val key = if (arity == 0) 0 else keyOfHeap(regs[0])
        var i = from
        while (i < size && !matches(clauses[i].key, key)) i++
        if (i == size) return false
        var j = i + 1
        while (j < size && !matches(clauses[j].key, key)) j++
        if (j < size) pushChoicePoint(predicate.functor, j, arity, barrier, 0)
        cutBarrier = barrier
        pc = clauses[i].start
        return true
    }

    private fun matches(
        clauseKey: Int,
        goalKey: Int,
    ): Boolean = clauseKey == 0 || goalKey == 0 || clauseKey == goalKey

    /**
     * Calls the predicate, builtin or primitive of [functor] on the registers, [cutBarrier] being the barrier of
     * the clause entered: [FAILED], [CONTINUE] (a builtin done, the code goes on at [pc]) or [JUMPED] (at [pc])
     */
    private fun call(
        functor: Int,
        arity: Int,
    ): Int {
        if (top > gcThreshold) collect(arity)
        if (functor < builtinTable.size) builtinTable[functor]?.let { return it.call(this) }
        program.predicateTable[functor]?.let { return if (tryClauses(it, 0, arity, cutBarrier)) JUMPED else FAILED }
        val primitive = primitiveFor(functor) ?: return FAILED
        return if (callPrimitive(primitive, functor, arity)) CONTINUE else FAILED
    }

    /** Calls the goal [w], a term on the heap, its arguments loaded in the registers */
    fun callTerm(w: Int): Int {
        val g = deref(w)
        val functor: Int
        val arity: Int
        when (tag(g)) {
            ATOM -> {
                functor = program.functor(payload(g), 0)
                arity = 0
            }
            STR -> {
                val a = payload(g)
                val f = heap[a]
                functor = payload(f)
                arity = program.arity(f)
                if (arity > regs.size) regs = regs.copyOf(arity * 2)
                for (i in 0 until arity) regs[i] = heap[a + 1 + i]
            }
            LIST -> {
                val a = payload(g)
                functor = fList
                arity = 2
                regs[0] = heap[a]
                regs[1] = heap[a + 1]
            }
            REF -> throw IllegalStateException("instantiation_error: the goal is a variable")
            else -> throw IllegalStateException("type_error(callable)")
        }
        return call(functor, arity)
    }

    // ---- the instructions

    /** Runs the instruction at [pc]: false on failure */
    fun execute(): Boolean {
        val code = program.code
        val at = pc
        when (code[at]) {
            Op.GET_X_VAR -> {
                regs[code[at + 1]] = regs[code[at + 2]]
                pc = at + 3
            }
            Op.GET_Y_VAR -> {
                local[frame + FRAME + code[at + 1]] = regs[code[at + 2]]
                pc = at + 3
            }
            Op.GET_X_VAL -> {
                if (!unify(regs[code[at + 1]], regs[code[at + 2]])) return false
                pc = at + 3
            }
            Op.GET_Y_VAL -> {
                if (!unify(local[frame + FRAME + code[at + 1]], regs[code[at + 2]])) return false
                pc = at + 3
            }
            Op.GET_CONST -> {
                if (!unifyConstant(code[at + 1], regs[code[at + 2]])) return false
                pc = at + 3
            }
            Op.GET_STR -> {
                val f = code[at + 1]
                val d = deref(regs[code[at + 2]])
                when (tag(d)) {
                    REF -> {
                        val h = alloc(1)
                        heap[h] = f
                        bind(payload(d), word(STR, h))
                        writing = true
                    }
                    STR -> {
                        val a = payload(d)
                        if (heap[a] != f) return false
                        next = a + 1
                        writing = false
                    }
                    else -> return false
                }
                pc = at + 3
            }
            Op.GET_LIST -> {
                val d = deref(regs[code[at + 1]])
                when (tag(d)) {
                    REF -> {
                        bind(payload(d), word(LIST, top))
                        writing = true
                    }
                    LIST -> {
                        next = payload(d)
                        writing = false
                    }
                    else -> return false
                }
                pc = at + 2
            }
            Op.UNIFY_X_VAR -> {
                if (writing) {
                    val h = alloc(1)
                    heap[h] = ref(h)
                    regs[code[at + 1]] = ref(h)
                } else {
                    regs[code[at + 1]] = heap[next++]
                }
                pc = at + 2
            }
            Op.UNIFY_Y_VAR -> {
                if (writing) {
                    val h = alloc(1)
                    heap[h] = ref(h)
                    local[frame + FRAME + code[at + 1]] = ref(h)
                } else {
                    local[frame + FRAME + code[at + 1]] = heap[next++]
                }
                pc = at + 2
            }
            Op.UNIFY_X_VAL -> {
                if (writing) {
                    heap[alloc(1)] = regs[code[at + 1]]
                } else if (!unify(regs[code[at + 1]], heap[next++])) {
                    return false
                }
                pc = at + 2
            }
            Op.UNIFY_Y_VAL -> {
                if (writing) {
                    heap[alloc(1)] = local[frame + FRAME + code[at + 1]]
                } else if (!unify(local[frame + FRAME + code[at + 1]], heap[next++])) {
                    return false
                }
                pc = at + 2
            }
            Op.UNIFY_CONST -> {
                if (writing) {
                    heap[alloc(1)] = code[at + 1]
                } else if (!unifyConstant(code[at + 1], heap[next++])) {
                    return false
                }
                pc = at + 2
            }
            Op.UNIFY_VOID -> {
                val n = code[at + 1]
                if (writing) {
                    val h = alloc(n)
                    for (i in h until h + n) heap[i] = ref(i)
                } else {
                    next += n
                }
                pc = at + 2
            }
            Op.PUT_X_VAR -> {
                val h = newVariable()
                regs[code[at + 1]] = ref(h)
                regs[code[at + 2]] = ref(h)
                pc = at + 3
            }
            Op.PUT_Y_VAR -> {
                val h = newVariable()
                local[frame + FRAME + code[at + 1]] = ref(h)
                regs[code[at + 2]] = ref(h)
                pc = at + 3
            }
            Op.PUT_X_VAL -> {
                regs[code[at + 2]] = regs[code[at + 1]]
                pc = at + 3
            }
            Op.PUT_Y_VAL -> {
                regs[code[at + 2]] = local[frame + FRAME + code[at + 1]]
                pc = at + 3
            }
            Op.PUT_CONST -> {
                regs[code[at + 2]] = code[at + 1]
                pc = at + 3
            }
            Op.PUT_STR -> {
                val h = alloc(1)
                heap[h] = code[at + 1]
                regs[code[at + 2]] = word(STR, h)
                pc = at + 3
            }
            Op.PUT_LIST -> {
                regs[code[at + 1]] = word(LIST, top)
                pc = at + 2
            }
            Op.SET_X_VAR -> {
                val h = alloc(1)
                heap[h] = ref(h)
                regs[code[at + 1]] = ref(h)
                pc = at + 2
            }
            Op.SET_Y_VAR -> {
                val h = alloc(1)
                heap[h] = ref(h)
                local[frame + FRAME + code[at + 1]] = ref(h)
                pc = at + 2
            }
            Op.SET_X_VAL -> {
                heap[alloc(1)] = regs[code[at + 1]]
                pc = at + 2
            }
            Op.SET_Y_VAL -> {
                heap[alloc(1)] = local[frame + FRAME + code[at + 1]]
                pc = at + 2
            }
            Op.SET_CONST -> {
                heap[alloc(1)] = code[at + 1]
                pc = at + 2
            }
            Op.SET_VOID -> {
                val n = code[at + 1]
                val h = alloc(n)
                for (i in h until h + n) heap[i] = ref(i)
                pc = at + 2
            }
            Op.ALLOCATE -> {
                allocate(code[at + 1])
                pc = at + 2
            }
            Op.DEALLOCATE -> {
                deallocate()
                pc = at + 1
            }
            Op.CALL -> {
                val functor = code[at + 1]
                pc = at + 2
                continuation = pc
                cutBarrier = cpTop
                return call(functor, program.functorArities[functor]) != FAILED
            }
            Op.EXECUTE -> {
                val functor = code[at + 1]
                cutBarrier = cpTop
                when (call(functor, program.functorArities[functor])) {
                    FAILED -> return false
                    CONTINUE -> pc = continuation
                    else -> Unit
                }
            }
            Op.PROCEED -> pc = continuation
            Op.CUT -> {
                cutTo(local[frame + 2])
                pc = at + 1
            }
            Op.GET_LEVEL -> {
                local[frame + FRAME + code[at + 1]] = Words.int(cpTop)
                pc = at + 2
            }
            Op.CUT_Y -> {
                cutTo(payload(local[frame + FRAME + code[at + 1]]))
                pc = at + 2
            }
            Op.TRY_ELSE -> {
                pushChoicePoint(RESUME, 0, 0, 0, code[at + 1])
                pc = at + 2
            }
            Op.TRUST_ME -> pc = at + 1
            Op.JUMP -> pc = code[at + 1]
            Op.FAIL -> return false
            Op.CALL_TERM -> {
                val goal = local[frame + FRAME + code[at + 1]]
                pc = at + 3
                continuation = pc
                cutBarrier = if (code[at + 2] == Op.TRANSPARENT) local[frame + 2] else cpTop
                return callTerm(goal) != FAILED
            }
            Op.EXECUTE_TERM -> {
                val goal = regs[code[at + 1]]
                if (code[at + 2] == Op.OPAQUE) cutBarrier = cpTop
                when (callTerm(goal)) {
                    FAILED -> return false
                    CONTINUE -> pc = continuation
                    else -> Unit
                }
            }
            Op.STOP -> stopped = true
            Op.INIT_Y -> {
                local[frame + FRAME + code[at + 1]] = ref(newVariable())
                pc = at + 2
            }
            else -> throw IllegalStateException("unknown instruction ${code[at]} at $at")
        }
        return true
    }

    /** Runs until a run reaches its end (true: a solution) or no choice point above [cpBase] is left */
    fun run(cpBase: Int): Boolean {
        while (true) {
            if (stopped) {
                stopped = false
                return true
            }
            if (!execute() && !backtrack(cpBase)) return false
        }
    }

    // ---- arithmetic

    fun eval(w0: Int): Long {
        val w = deref(w0)
        return when (tag(w)) {
            INT -> payload(w).toLong()
            BIG -> program.bigs[payload(w)]
            STR -> {
                val a = payload(w)
                when (payload(heap[a])) {
                    fPlus -> eval(heap[a + 1]) + eval(heap[a + 2])
                    fMinus -> eval(heap[a + 1]) - eval(heap[a + 2])
                    fTimes -> eval(heap[a + 1]) * eval(heap[a + 2])
                    fIntDiv -> eval(heap[a + 1]) / eval(heap[a + 2])
                    fMod -> eval(heap[a + 1]).mod(eval(heap[a + 2]))
                    fNeg -> -eval(heap[a + 1])
                    else -> throw IllegalStateException("type_error(evaluable, ${program.functorName(heap[a])})")
                }
            }
            REF -> throw IllegalStateException("instantiation_error in arithmetic")
            else -> throw IllegalStateException("type_error(evaluable)")
        }
    }

    // ---- pinned roots

    private fun pinWord(w: Int): Int {
        if (pinnedWordsTop == pinnedWords.size) pinnedWords = pinnedWords.copyOf(pinnedWords.size * 2)
        pinnedWords[pinnedWordsTop] = w
        return pinnedWordsTop++
    }

    // ---- findall

    private fun findall(
        template: Int,
        goal: Int,
        result: Int,
    ): Int {
        val pinnedBase = pinnedWordsTop
        val pinnedTemplate = pinWord(template)
        val pinnedResult = pinWord(result)
        // The barrier choice point: the state before the goal, resumed at the instruction after the call
        pushChoicePoint(BARRIER, 0, 0, 0, continuation)
        val cpBase = cpTop
        continuation = stopAddress
        cutBarrier = cpTop
        var found =
            when (callTerm(goal)) {
                FAILED -> backtrack(cpBase) && run(cpBase)
                CONTINUE -> {
                    pc = continuation
                    run(cpBase)
                }
                else -> run(cpBase)
            }
        var bag = IntArray(64)
        var bagTop = 0
        var roots = IntArray(16)
        var count = 0
        while (found) {
            if (count == roots.size) roots = roots.copyOf(count * 2)
            val copier = Copier(bag, bagTop)
            val root = copier.copy(deref(pinnedWords[pinnedTemplate]))
            bag = copier.bag
            bagTop = copier.top
            roots[count++] = root
            found = backtrack(cpBase) && run(cpBase)
        }
        restoreTop()
        popChoicePoint()
        val resultWord = pinnedWords[pinnedResult]
        pinnedWordsTop = pinnedBase
        // The bag onto the heap, its addresses shifted
        val base = alloc(bagTop)
        for (i in 0 until bagTop) {
            val w = bag[i]
            heap[base + i] = if (tag(w) == REF || tag(w) == STR || tag(w) == LIST) word(tag(w), payload(w) + base) else w
        }
        var list = program.emptyList
        for (k in count - 1 downTo 0) {
            val cell = alloc(2)
            val r = roots[k]
            heap[cell] = if (tag(r) == REF || tag(r) == STR || tag(r) == LIST) word(tag(r), payload(r) + base) else r
            heap[cell + 1] = list
            list = word(LIST, cell)
        }
        return if (unify(resultWord, list)) CONTINUE else FAILED
    }

    // A copy of a term of the heap into a bag of its own (relative addresses), its unbound variables fresh
    private inner class Copier(
        var bag: IntArray,
        var top: Int,
    ) {
        private val variables = HashMap<Int, Int>()

        private fun allocBag(cells: Int): Int {
            if (top + cells > bag.size) bag = bag.copyOf(maxOf(bag.size * 2, top + cells))
            val start = top
            top += cells
            return start
        }

        fun copy(w0: Int): Int {
            val w = deref(w0)
            return when (tag(w)) {
                REF ->
                    ref(
                        variables.getOrPut(payload(w)) {
                            val a = allocBag(1)
                            bag[a] = ref(a)
                            a
                        },
                    )
                STR -> {
                    val a = payload(w)
                    val n = program.arity(heap[a])
                    val s = allocBag(n + 1)
                    bag[s] = heap[a]
                    for (i in 1..n) {
                        val arg = copy(heap[a + i])
                        bag[s + i] = arg
                    }
                    word(STR, s)
                }
                LIST -> {
                    val first = allocBag(2)
                    var s = first
                    var a = payload(w)
                    while (true) {
                        val head = copy(heap[a])
                        bag[s] = head
                        val t = deref(heap[a + 1])
                        if (tag(t) == LIST) {
                            val s2 = allocBag(2)
                            bag[s + 1] = word(LIST, s2)
                            s = s2
                            a = payload(t)
                        } else {
                            val tail = copy(t)
                            bag[s + 1] = tail
                            break
                        }
                    }
                    word(LIST, first)
                }
                else -> w
            }
        }
    }

    // ---- the facade: the primitives of the libraries

    private fun primitiveFor(functor: Int): Primitive? {
        val environment = environment ?: return null
        if (functor >= primitiveTable.size) {
            val size = maxOf(program.functorNames.size, functor + 1)
            primitiveTable = primitiveTable.copyOf(size)
            primitiveLooked = primitiveLooked.copyOf(size)
        }
        if (!primitiveLooked[functor]) {
            primitiveLooked[functor] = true
            primitiveTable[functor] =
                environment.libraries.primitives[Signature(program.nameOfFunctor(functor), program.functorArities[functor])]
        }
        return primitiveTable[functor]
    }

    // Calls [primitive] on the registers: the goal as terms, the first answer unified back, the others left as a
    // choice point resuming at the continuation (the next instruction of a call, see Op.CALL)
    private fun callPrimitive(
        primitive: Primitive,
        functor: Int,
        arity: Int,
    ): Boolean {
        val names = HashMap<Int, Var>()
        val arguments = List(arity) { toTerm(regs[it], names) }
        val name = program.nameOfFunctor(functor)
        val goal = if (arity == 0) Atom.of(name) else Struct.of(name, arguments)
        val context: ExecutionContext = PrologExecutionContext(environment!!, procedure = goal)
        val request = Solve.Request(Signature(name, arity), arguments, context)
        val responses = primitive.solve(request).iterator()
        if (!responses.hasNext()) return false
        val response = responses.next()
        val variables = arrayOfNulls<Var>(names.size)
        val cells = IntArray(names.size)
        var i = 0
        for ((cell, variable) in names) {
            variables[i] = variable
            cells[i] = cell
            i++
        }
        @Suppress("UNCHECKED_CAST")
        val call = PrimitiveCall(responses, variables as Array<Var>, cells)
        if (responses.hasNext()) {
            pushChoicePoint(PRIMITIVE, primitiveCalls.size, 0, 0, continuation)
            primitiveCalls.add(call)
        }
        return applyResponse(response, call)
    }

    // The answer of a primitive: its side effects applied, its bindings unified with the cells of the goal
    private fun applyResponse(
        response: Solve.Response,
        call: PrimitiveCall,
    ): Boolean {
        val solution = response.solution
        if (response.sideEffects.isNotEmpty()) applySideEffects(response.sideEffects)
        return when (solution) {
            is Solution.Yes -> {
                val cells = HashMap<Var, Int>(call.variables.size * 2)
                for (i in call.variables.indices) cells[call.variables[i]] = call.cells[i]
                var ok = true
                for ((variable, term) in solution.substitution) {
                    val cell = cells[variable] ?: continue
                    if (!unify(ref(cell), fromTerm(term, cells))) {
                        ok = false
                        break
                    }
                }
                ok
            }
            is Solution.No -> false
            is Solution.Halt -> throw Halt(solution.exception)
        }
    }

    private fun applySideEffects(sideEffects: List<SideEffect>) {
        val environment = environment!!
        for (effect in sideEffects) {
            when (effect) {
                is SideEffect.AddDynamicClauses -> {
                    for (clause in effect.clauses) {
                        if (effect.onTop) environment.dynamicKb.assertA(clause) else environment.dynamicKb.assertZ(clause)
                        program.add(clause, dynamic = true, onTop = effect.onTop)
                    }
                    prepare()
                }
                is SideEffect.RemoveDynamicClauses -> {
                    for (clause in effect.clauses) {
                        environment.dynamicKb.retract(clause)
                        program.remove(clause)
                    }
                }
                else -> throw IllegalStateException("side effect not supported by the machine: $effect")
            }
        }
    }

    // ---- garbage collection

    /** Collects the heap cells no longer reachable, [arity] registers holding the arguments of the call at hand */
    fun collect(arity: Int) {
        collections++
        collectedFrom = top
        if (forward.size < top) forward = IntArray(maxOf(top, heap.size))
        forward.fill(-1, 0, top)
        // Marking: the cells reachable from the roots
        for (i in 0 until arity) markWord(regs[i])
        if (frameVisited.size < local.size) frameVisited = BooleanArray(local.size)
        frameVisited.fill(false, 0, local.size)
        markFrames(frame)
        for (cp in 0 until cpTop) markFrames(cps[cp * CP_SIZE + 3])
        for (i in 0 until cpArgsTop) markWord(cpArgs[i])
        for (i in 0 until trailTop) markCell(trail[i])
        for (i in 0 until pinnedWordsTop) markWord(pinnedWords[i])
        for (call in primitiveCalls) for (c in call.cells) markCell(c)
        // The new addresses: those of the cells kept, and for the others the count of cells kept below (negated)
        var n = 0
        for (i in 0 until top) {
            if (forward[i] != -1) {
                forward[i] = n
                n++
            } else {
                forward[i] = -n - 1
            }
        }
        collectedTo = n
        // The roots relocated
        for (i in 0 until arity) regs[i] = relocated(regs[i])
        frameVisited.fill(false, 0, local.size)
        relocateFrames(frame)
        for (cp in 0 until cpTop) relocateFrames(cps[cp * CP_SIZE + 3])
        for (i in 0 until cpArgsTop) cpArgs[i] = relocated(cpArgs[i])
        for (i in 0 until trailTop) trail[i] = forward[trail[i]]
        for (i in 0 until pinnedWordsTop) pinnedWords[i] = relocated(pinnedWords[i])
        for (call in primitiveCalls) for (i in call.cells.indices) call.cells[i] = forward[call.cells[i]]
        for (cp in 0 until cpTop) {
            val base = cp * CP_SIZE
            cps[base + 1] = newAddress(cps[base + 1])
        }
        // The cells kept slide down, their words relocated
        for (i in 0 until top) {
            val f = forward[i]
            if (f >= 0) heap[f] = relocated(heap[i])
        }
        top = n
        heapBarrier = if (cpTop > 0) cps[(cpTop - 1) * CP_SIZE + 1] else 0
        gcThreshold = maxOf(gcMinimum, 4 * n)
    }

    // The slots of the frames from [start] up (each once)
    private fun markFrames(start: Int) {
        var e = start
        while (e != NONE && !frameVisited[e]) {
            frameVisited[e] = true
            val slots = local[e + 3]
            for (i in 0 until slots) markWord(local[e + FRAME + i])
            e = local[e]
        }
    }

    private fun relocateFrames(start: Int) {
        var e = start
        while (e != NONE && !frameVisited[e]) {
            frameVisited[e] = true
            val slots = local[e + 3]
            for (i in 0 until slots) local[e + FRAME + i] = relocated(local[e + FRAME + i])
            e = local[e]
        }
    }

    // The new address of a mark [address] (the first cell at or after it kept)
    private fun newAddress(address: Int): Int {
        if (address >= collectedFrom) return collectedTo
        val f = forward[address]
        return if (f >= 0) f else -f - 1
    }

    private fun relocated(w: Int): Int = if (tag(w) == REF || tag(w) == STR || tag(w) == LIST) word(tag(w), forward[payload(w)]) else w

    private fun markCell(c: Int) {
        if (forward[c] != -1) return
        forward[c] = 0
        var sp = 0
        markStack[sp++] = c
        while (sp > 0) {
            val cell = markStack[--sp]
            val w = heap[cell]
            when (tag(w)) {
                REF -> {
                    val a = payload(w)
                    if (forward[a] == -1) {
                        forward[a] = 0
                        sp = push(sp, a)
                    }
                }
                STR -> {
                    val a = payload(w)
                    if (forward[a] == -1) {
                        forward[a] = 0
                        val n = program.arity(heap[a])
                        for (i in 1..n) {
                            if (forward[a + i] == -1) {
                                forward[a + i] = 0
                                sp = push(sp, a + i)
                            }
                        }
                    }
                }
                LIST -> {
                    val a = payload(w)
                    for (i in 0..1) {
                        if (forward[a + i] == -1) {
                            forward[a + i] = 0
                            sp = push(sp, a + i)
                        }
                    }
                }
                else -> Unit
            }
        }
    }

    private fun push(
        sp: Int,
        cell: Int,
    ): Int {
        if (sp == markStack.size) markStack = markStack.copyOf(sp * 2)
        markStack[sp] = cell
        return sp + 1
    }

    private fun markWord(w: Int) {
        when (tag(w)) {
            REF -> markCell(payload(w))
            STR -> {
                val a = payload(w)
                if (forward[a] == -1) {
                    forward[a] = 0
                    val n = program.arity(heap[a])
                    for (i in 1..n) markCell(a + i)
                }
            }
            LIST -> {
                markCell(payload(w))
                markCell(payload(w) + 1)
            }
            else -> Unit
        }
    }

    // ---- terms in and out

    /** [term] on the heap, its variables as cells (those of [variables], allocated as met) */
    fun fromTerm(
        term: Term,
        variables: HashMap<Var, Int>,
    ): Int =
        when (term) {
            is Var -> ref(variables.getOrPut(term) { newVariable() })
            is Integer -> program.intWord(term.value.toLong())
            is Real -> program.floatWord(term.value.toDouble())
            is Atom -> program.atomWord(term.value)
            is Cons -> {
                val first = alloc(2)
                var s = first
                var list: Term = term
                while (true) {
                    val cons = list as Cons
                    val head = fromTerm(cons.head, variables)
                    heap[s] = head
                    val tail = cons.tail
                    if (tail is Cons) {
                        val s2 = alloc(2)
                        heap[s + 1] = word(LIST, s2)
                        s = s2
                        list = tail
                    } else {
                        val t = fromTerm(tail, variables)
                        heap[s + 1] = t
                        break
                    }
                }
                word(LIST, first)
            }
            is Struct -> {
                val n = term.arity
                val s = alloc(n + 1)
                heap[s] = word(FUN, program.functor(term.functor, n))
                for (i in 0 until n) {
                    val arg = fromTerm(term[i], variables)
                    heap[s + 1 + i] = arg
                }
                word(STR, s)
            }
            else -> throw IllegalArgumentException("not a term of the machine: $term")
        }

    /** The term of heap word [w0], its unbound cells as the variables of [names] */
    fun toTerm(
        w0: Int,
        names: HashMap<Int, Var>,
    ): Term {
        val w = deref(w0)
        return when (tag(w)) {
            REF -> names.getOrPut(payload(w)) { Var.of("_G${payload(w)}") }
            ATOM -> Atom.of(program.atoms[payload(w)])
            INT -> Integer.of(payload(w))
            BIG -> Integer.of(program.bigs[payload(w)])
            FLT -> Real.of(program.floats[payload(w)])
            STR -> {
                val a = payload(w)
                val n = program.arity(heap[a])
                Struct.of(program.functorName(heap[a]), List(n) { toTerm(heap[a + 1 + it], names) })
            }
            LIST -> {
                val heads = ArrayList<Term>()
                var a = payload(w)
                while (true) {
                    heads.add(toTerm(heap[a], names))
                    val t = deref(heap[a + 1])
                    if (tag(t) == LIST) {
                        a = payload(t)
                    } else {
                        return LogicList.from(heads, toTerm(t, names))
                    }
                }
                @Suppress("UNREACHABLE_CODE")
                throw IllegalStateException()
            }
            else -> throw IllegalStateException("not a term: $w")
        }
    }

    /** The machine emptied: a top-level query starts from nothing */
    fun reset() {
        top = 0
        heapBarrier = 0
        trailTop = 0
        frame = NONE
        cpTop = 0
        cpArgsTop = 0
        pinnedWordsTop = 0
        primitiveCalls.clear()
        stopped = false
    }

    /** The solutions of [query]: each the bindings of its variables, until none is left */
    fun solve(query: Struct): Sequence<Map<Var, Term>> =
        sequence {
            reset()
            val compiled = program.compileQuery(query)
            prepare()
            // (the cells of the variables of the query, as the arguments of its code, pinned: the collector moves them)
            val pins = HashMap<Var, Int>()
            for ((i, variable) in compiled.variables.withIndex()) {
                val cell = newVariable()
                regs[i] = ref(cell)
                pins[variable] = pinWord(ref(cell))
            }
            continuation = stopAddress
            cutBarrier = 0
            pc = compiled.start
            var found = run(0)
            while (found) {
                val names = HashMap<Int, Var>()
                // (the variables left unbound are not bindings)
                yield(
                    pins.entries
                        .filter { (_, pin) -> tag(deref(pinnedWords[pin])) != REF }
                        .associate { (variable, pin) -> variable to toTerm(pinnedWords[pin], names) },
                )
                found = backtrack(0) && run(0)
            }
        }

    companion object {
        const val NONE = -1
        const val FRAME = 4
        const val CP_SIZE = 12

        // The slots of a frame before the clause sets them
        private const val UNSET = (0 shl Words.TAG_BITS) or INT

        // The functors of the choice points that are not clause alternatives
        const val BARRIER = -2
        const val RESUME = -3
        const val PRIMITIVE = -4

        // The outcomes of a call
        const val FAILED = 0
        const val CONTINUE = 1
        const val JUMPED = 2

        // The heap cells above which a call collects
        const val GC_MIN = 1 shl 20
    }
}
