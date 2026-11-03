package it.unibo.tuprolog.argumentation.core.engine.vm

import it.unibo.tuprolog.argumentation.core.engine.ClauseObservers
import it.unibo.tuprolog.argumentation.core.engine.ClauseSource
import it.unibo.tuprolog.argumentation.core.engine.DelegatedCall
import it.unibo.tuprolog.argumentation.core.engine.ExecutionEnvironment
import it.unibo.tuprolog.argumentation.core.engine.Inlining
import it.unibo.tuprolog.argumentation.core.engine.NativeCall
import it.unibo.tuprolog.argumentation.core.engine.NativePrimitive
import it.unibo.tuprolog.argumentation.core.engine.NativeResume
import it.unibo.tuprolog.argumentation.core.engine.PrologExecutionContext
import it.unibo.tuprolog.argumentation.core.engine.Sourcing
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
import it.unibo.tuprolog.core.Clause
import it.unibo.tuprolog.core.Cons
import it.unibo.tuprolog.core.Indicator
import it.unibo.tuprolog.core.Integer
import it.unibo.tuprolog.core.Real
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.Var
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.Solution
import it.unibo.tuprolog.solve.currentTimeInstant
import it.unibo.tuprolog.solve.exception.LogicError
import it.unibo.tuprolog.solve.exception.ResolutionException
import it.unibo.tuprolog.solve.exception.TimeOutException
import it.unibo.tuprolog.solve.exception.error.MessageError
import it.unibo.tuprolog.solve.exception.error.SystemError
import it.unibo.tuprolog.solve.library.Runtime
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
 * The predicates a goal is resolved with are those of the [scope] of the libraries the resolution runs with, and
 * those of the theory of the user when it is visible: a module call ([Inlining]) switches them for its goal and
 * back when the goal is done (a frame of its own holds the scope to restore). A lookup ([Sourcing]) resolves its
 * goal with clauses read when it is made.
 *
 * An exception is a ball thrown through the code ([Raised]): the choice points are unwound to the nearest catch/3
 * ([CATCH]) whose catcher unifies with it, and its recovery runs in its place; a ball no catch takes halts the run.
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
) : NativeCall {
    var heap = IntArray(1 shl 16)
    var top = 0

    // What a cell holding an unbound variable is called when the term is read: 0 nothing, a name atom and one for a
    // variable the code made (fresh as read), minus an index and one for the variable object the cell was made for
    // (the same object as read again: a stored term read twice gives the same variables, as the engine does)
    private var varInfo = IntArray(1 shl 16)
    private val varObjects = ArrayList<Var>()

    // The cells of the variables of the query (the first ones), and when each was bound (the engine reported the
    // bindings of a solution in the order they were made)
    private var queryCells = 0
    private var queryBound = IntArray(16)
    private var bindings = 0

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

    /** The libraries the resolution runs with, and whether the theory of the user is visible */
    var scope: Program.Scope = program.theory
    var theoryVisible = true

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

    private val builtins = HashMap<Int, Builtin>()
    private var builtinTable = arrayOfNulls<Builtin>(0)

    /** The facade: the environment the primitives of the libraries are called in, as their side effects left it */
    var environment: ExecutionEnvironment? = null
        private set

    // The time the run must end by, and the calls made since it was last checked
    private var deadline = Long.MAX_VALUE
    private var callsSinceCheck = 0

    // Whether the theory of the user is visible when a run starts
    private var initialTheoryVisible = true

    // The calls with alternatives left that are not clauses: of primitives (their responses) and of lookups (their
    // clauses), referred to by the choice points
    private val calls = ArrayList<Any>()

    // Garbage collection
    private var gcThreshold = gcMinimum
    private var marks = LongArray(0)
    private var counts = IntArray(0)
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

    // A call of a primitive with answers left: its responses, the cells of the variables of its goal, and the error
    // raised computing the next answer ahead, if any (raised when the resolution backtracks into that answer)
    private class PrimitiveCall(
        val responses: Iterator<Solve.Response>,
        val variables: Array<Var>,
        val cells: IntArray,
        var pending: Exception? = null,
    )

    // A lookup with clauses left to try
    private class SourcedCall(
        val clauses: List<Clause>,
    )

    // The arity of the native primitive being called, and the variables its [term]s gave the cells (one call)
    private var nativeArity = 0
    private var nativeFunctor = 0
    private var nativeNames: HashMap<Int, Var>? = null

    /** A ball thrown through the code: the term, and the exception it came from, if any */
    class Raised(
        val ball: Term,
        val exception: ResolutionException? = null,
    ) : RuntimeException(ball.toString())

    /** A resolution halted by an exception no catch/3 took */
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

    private val fIs = program.functor("is", 2)
    private val wPlus = program.functorWord("+", 2)
    private val wMinus = program.functorWord("-", 2)
    private val wTimes = program.functorWord("*", 2)
    private val wIntDiv = program.functorWord("//", 2)
    private val wMod = program.functorWord("mod", 2)
    private val wNeg = program.functorWord("-", 1)
    private val comparisonFunctors =
        intArrayOf(
            0,
            program.functor("<", 2),
            program.functor(">", 2),
            program.functor("=<", 2),
            program.functor(">=", 2),
            program.functor("=:=", 2),
            program.functor("=\\=", 2),
        )

    // The stacks of the inline arithmetic: the values being computed, the words of an expression being built for the
    // builtin when the machine cannot compute it, and whether the last evaluation could not
    private var values = LongArray(32)
    private var exprWords = IntArray(32)
    private var evalFailed = false

    private val fIfThen = program.functorWord("->", 2)
    private val fList = program.functor(".", 2)
    private val failGoal = program.atomWord("fail")

    // The code of the control constructs called as goals at run time, of the end of a run, of the return from an
    // inlined goal
    private val compiler = Compiler(program)
    private val stopAddress = compiler.stopBlock()
    private val conjunctionBlock = compiler.conjunctionBlock()
    private val disjunctionBlock = compiler.disjunctionBlock()
    private val ifThenElseBlock = compiler.ifThenElseBlock()
    private val negationBlock = compiler.negationBlock()
    private val restoreScopeBlock = compiler.restoreScopeBlock()

    init {
        builtin("true", 0) { CONTINUE }
        builtin("fail", 0) { FAILED }
        builtin("false", 0) { FAILED }
        builtin("!", 0) { m ->
            m.cutTo(m.cutBarrier)
            CONTINUE
        }
        builtin("=", 2) { m -> if (m.unify(m.regs[0], m.regs[1])) CONTINUE else FAILED }
        arithmetic("is") { m -> if (m.unify(m.regs[0], m.program.intWord(m.eval(m.regs[1])))) CONTINUE else FAILED }
        arithmetic("<") { m -> if (m.eval(m.regs[0]) < m.eval(m.regs[1])) CONTINUE else FAILED }
        arithmetic(">") { m -> if (m.eval(m.regs[0]) > m.eval(m.regs[1])) CONTINUE else FAILED }
        arithmetic("=<") { m -> if (m.eval(m.regs[0]) <= m.eval(m.regs[1])) CONTINUE else FAILED }
        arithmetic(">=") { m -> if (m.eval(m.regs[0]) >= m.eval(m.regs[1])) CONTINUE else FAILED }
        arithmetic("=:=") { m -> if (m.eval(m.regs[0]) == m.eval(m.regs[1])) CONTINUE else FAILED }
        arithmetic("=\\=") { m -> if (m.eval(m.regs[0]) != m.eval(m.regs[1])) CONTINUE else FAILED }
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
        builtin("catch", 3) { m -> m.catch() }
        builtin("throw", 1) { m ->
            val ball = m.deref(m.regs[0])
            if (tag(ball) == REF) throw Raised(error(Atom.of("instantiation_error"), "throw/1"))
            throw Raised(m.toTerm(ball, HashMap()))
        }
    }

    fun builtin(
        name: String,
        arity: Int,
        implementation: Builtin,
    ) {
        builtins[program.functor(name, arity)] = implementation
    }

    // An arithmetic builtin: [native] on the integers and the operators the machine evaluates itself, the primitive
    // of the libraries (tuProlog's evaluation, floats and every function) on anything else
    private fun arithmetic(
        name: String,
        native: Builtin,
    ) {
        val functor = program.functor(name, 2)
        builtins[functor] =
            Builtin { m ->
                try {
                    native.call(m)
                } catch (_: NotNative) {
                    val primitive =
                        m.scope.primitive(functor)
                            ?: throw Raised(error(Struct.of("existence_error", Atom.of("procedure"), Indicator.of(name, 2)), "$name/2"))
                    m.callPrimitive(primitive, functor, 2)
                }
            }
    }

    // An expression the machine does not evaluate itself (see arithmetic)
    private class NotNative : RuntimeException()

    /** Prepares the dispatch tables: to call once the program is loaded */
    fun prepare() {
        val functors = program.functorNames.size
        builtinTable = arrayOfNulls(functors)
        for ((functor, builtin) in builtins) builtinTable[functor] = builtin
        if (program.maxRegister + 1 > regs.size) regs = regs.copyOf(program.maxRegister + 1)
    }

    /**
     * The primitives and the clauses of the libraries of [environment] answer the goals the theory and the builtins
     * do not
     */
    fun facade(environment: ExecutionEnvironment) {
        this.environment = environment
        scope = program.scopeOf(environment.libraries)
        prepare()
    }

    private fun jump(address: Int): Int {
        pc = address
        return JUMPED
    }

    // The environment as a primitive sees it where it is called: within a module call, the libraries of the module
    // and the theory of the user hidden (the solvers it creates for its sub-goals see the same), the root's otherwise;
    // kept per scope and visibility until the root changes
    private val environmentsSeen = HashMap<Int, ExecutionEnvironment>()

    private fun environmentSeen(): ExecutionEnvironment {
        val base = environment!!
        val libraries = scope.libraries
        if ((libraries == null || libraries === base.libraries) && base.theoryVisible == theoryVisible) return base
        val key = scope.id * 2 + (if (theoryVisible) 1 else 0)
        val cached = environmentsSeen[key]
        if (cached != null && cached.root === base) return cached
        val made = base.copy(libraries = libraries ?: base.libraries, theoryVisible = theoryVisible, root = base)
        environmentsSeen[key] = made
        return made
    }

    // ---- heap

    fun alloc(cells: Int): Int {
        if (top + cells > heap.size) grow(top + cells)
        val start = top
        top += cells
        return start
    }

    private fun grow(cells: Int) {
        val size = maxOf(heap.size * 2, cells)
        heap = heap.copyOf(size)
        varInfo = varInfo.copyOf(size)
    }

    // A cell holding [w] allocated at the top (`heap[alloc(1)] = w` would store into the array read before alloc
    // grew it)
    private fun pushCell(w: Int) {
        if (top == heap.size) grow(top + 1)
        heap[top++] = w
    }

    /** A fresh unbound variable, called by [info] when read (see varInfo) */
    fun newVariable(info: Int = 0): Int {
        val a = alloc(1)
        heap[a] = ref(a)
        varInfo[a] = info
        return a
    }

    // A fresh unbound variable at the top, as the structure being built asks (see Op.UNIFY_X_VAR)
    private fun pushVariable(info: Int): Int {
        val a = alloc(1)
        heap[a] = ref(a)
        varInfo[a] = info
        return a
    }

    override fun deref(w0: Int): Int {
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
        if (address < queryCells) queryBound[address] = ++bindings
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

    override fun unify(
        x0: Int,
        y0: Int,
    ): Boolean {
        // The plain cases first, without the stack: the same word, a variable on either side, two constants
        val x1 = deref(x0)
        val y1 = deref(y0)
        if (x1 == y1) return true
        val tx1 = tag(x1)
        val ty1 = tag(y1)
        if (tx1 == REF) {
            if (ty1 == REF) bindVariables(payload(x1), payload(y1)) else bind(payload(x1), y1)
            return true
        }
        if (ty1 == REF) {
            bind(payload(y1), x1)
            return true
        }
        if (tx1 != ty1 || (tx1 != STR && tx1 != LIST)) return false
        var sp = 0
        pdl[sp++] = x1
        pdl[sp++] = y1
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

    // Two variables met: the first bound to the second (the engine bound the left one to the right: X = Y binds X;
    // either way is safe here, both cells being on the heap and the older one trailed)
    private fun bindVariables(
        a: Int,
        b: Int,
    ) {
        bind(a, ref(b))
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
        key: Int = 0,
        source: Int = MERGED,
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
        cps[base + 10] = calls.size
        cps[base + 11] = arity
        cps[base + 12] = key
        cps[base + 13] = scope.id
        cps[base + 14] = if (theoryVisible) 1 else 0
        cps[base + 15] = pinnedWordsTop
        cps[base + 16] = source
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
        scope = program.scope(cps[base + 13])
        theoryVisible = cps[base + 14] != 0
        pinnedWordsTop = cps[base + 15]
    }

    private fun popChoicePoint() {
        cpTop--
        val base = cpTop * CP_SIZE
        cpArgsTop = cps[base + 9]
        truncateCalls(cps[base + 10])
        heapBarrier = if (cpTop > 0) cps[(cpTop - 1) * CP_SIZE + 1] else 0
    }

    private fun truncateCalls(size: Int) {
        while (calls.size > size) calls.removeAt(calls.size - 1)
    }

    fun cutTo(barrier: Int) {
        if (cpTop > barrier) {
            val base = barrier * CP_SIZE
            cpArgsTop = cps[base + 9]
            truncateCalls(cps[base + 10])
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
                CATCH -> popChoicePoint()
                PRIMITIVE -> {
                    val call = calls[nextClause] as PrimitiveCall
                    call.pending?.let {
                        popChoicePoint()
                        throw raised(it)
                    }
                    val response = nextResponse(call.responses)
                    if (!hasNextResponse(call)) popChoicePoint()
                    if (applyResponse(response, call)) return true
                }
                NATIVE -> {
                    val resume = calls[nextClause] as NativeResume
                    val argsBase = cps[base + 9]
                    val arity = cps[base + 11]
                    cpArgs.copyInto(regs, 0, argsBase, argsBase + arity)
                    popChoicePoint()
                    nativeArity = arity
                    nativeNames = null
                    val outcome =
                        try {
                            resume.resume(this)
                        } catch (raised: Raised) {
                            throw raised
                        } catch (halt: Halt) {
                            throw halt
                        } catch (exception: Exception) {
                            throw raised(exception)
                        }
                    when (outcome) {
                        FAILED -> Unit
                        CONTINUE -> {
                            pc = continuation
                            return true
                        }
                        else -> return true
                    }
                }
                SOURCED -> {
                    val call = calls[nextClause] as SourcedCall
                    val argsBase = cps[base + 9]
                    val arity = cps[base + 11]
                    val from = cps[base + 12]
                    cpArgs.copyInto(regs, 0, argsBase, argsBase + arity)
                    popChoicePoint()
                    if (trySourced(call, from, arity, nextClause) != FAILED) return true
                }
                else -> {
                    val argsBase = cps[base + 9]
                    val arity = cps[base + 11]
                    val key = cps[base + 12]
                    val source = cps[base + 16]
                    cpArgs.copyInto(regs, 0, argsBase, argsBase + arity)
                    popChoicePoint()
                    val predicate = if (source == THEORY) program.theory.predicate(functor)!! else predicateOf(functor)!!
                    if (tryClauses(predicate, nextClause, arity, barrier, key, source)) return true
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

    // The predicate of [functor] the resolution sees: the theory of the user's, if visible and it defines it (the
    // theory is a module of its own, whatever the libraries define under the same name), else the libraries'
    private fun predicateOf(functor: Int): Program.Predicate? =
        (if (theoryVisible) program.theory.predicate(functor) else null) ?: scope.predicate(functor)

    // Enters the first clause of [predicate] from the position [from] among the candidates for [key] (the first
    // argument's, computed when [from] is 0), leaving a choice point if another may match (the keys of every
    // argument of its head are matched against the arguments, as the engine left no choice point for a clause whose
    // head did not unify); false when none does
    private fun tryClauses(
        predicate: Program.Predicate,
        from: Int,
        arity: Int,
        barrier: Int,
        key0: Int,
        source: Int,
    ): Boolean {
        val key = if (from == 0 && arity > 0) keyOfHeap(regs[0]) else key0
        val n = predicate.size
        if (key != 0 && n >= Program.Predicate.INDEXED_FROM) return tryIndexed(predicate, from, arity, barrier, key, source)
        // The clauses in order, over the flat tables: the first candidate by its first argument, its head deciding
        // the rest; the lookahead for another by every argument the clause keys
        val firstKeys = predicate.firstKeys
        var i = from
        if (n - from > 1) {
            while (i < n && !matches(firstKeys[i], key)) i++
            if (i == n) return false
            var j = i + 1
            if (predicate.deep) {
                val clauses = predicate.clauses
                while (j < n && !matches(clauses[j], key, arity)) j++
            } else {
                while (j < n && !matches(firstKeys[j], key)) j++
            }
            if (j < n) pushChoicePoint(predicate.functor, j, arity, barrier, 0, key, source)
        } else if (i == n) {
            return false
        }
        cutBarrier = barrier
        if (ClauseObservers.active) ClauseObservers.resolved(predicate.clauses[i].source)
        pc = predicate.starts[i]
        return true
    }

    // The candidates of [key] (the clauses of an indexed predicate it may match), in order
    private fun tryIndexed(
        predicate: Program.Predicate,
        from: Int,
        arity: Int,
        barrier: Int,
        key: Int,
        source: Int,
    ): Boolean {
        val clauses = predicate.clauses
        val candidates = predicate.candidates(key)
        val size = candidates.size
        var i = from
        if (size - from > 1) {
            while (i < size && !matches(clauses[candidates[i]].key, key)) i++
            if (i == size) return false
            var j = i + 1
            while (j < size && !matches(clauses[candidates[j]], key, arity)) j++
            if (j < size) pushChoicePoint(predicate.functor, j, arity, barrier, 0, key, source)
        } else if (i == size) {
            return false
        }
        cutBarrier = barrier
        val clause = clauses[candidates[i]]
        if (ClauseObservers.active) ClauseObservers.resolved(clause.source)
        pc = clause.start
        return true
    }

    private fun matches(
        clause: Program.ClauseCode,
        key: Int,
        arity: Int,
    ): Boolean {
        val keys = clause.keys
        if (arity == 0) return true
        if (!matches(keys[0], key)) return false
        for (k in 1 until arity) {
            val ck = keys[k]
            if (ck != 0 && !matches(ck, keyOfHeap(regs[k]))) return false
        }
        return true
    }

    private fun matches(
        clauseKey: Int,
        goalKey: Int,
    ): Boolean = clauseKey == 0 || goalKey == 0 || clauseKey == goalKey

    /**
     * Calls the predicate, builtin or primitive of [functor] on the registers, [cutBarrier] being the barrier a cut
     * called as the goal itself cuts to: [FAILED], [CONTINUE] (a builtin done, the code goes on at [pc]) or [JUMPED]
     * (at [pc])
     */
    private fun call(
        functor: Int,
        arity: Int,
    ): Int {
        if (top > gcThreshold || ++callsSinceCheck >= CHECK_EVERY) housekeeping(arity)
        if (functor < builtinTable.size) {
            val builtin = builtinTable[functor]
            if (builtin != null) return callBuiltin(builtin, functor, arity)
        }
        // (a cut in a clause of the predicate cuts its alternatives alone, whatever the goal is transparent to)
        val predicate = predicateOf(functor) ?: return callPrimitiveOf(functor, arity)
        return if (tryClauses(predicate, 0, arity, cpTop, 0, MERGED)) JUMPED else FAILED
    }

    // Every so many calls, or the heap above its threshold: the collection, the deadline and the interruption
    private fun housekeeping(arity: Int) {
        if (top > gcThreshold) collect(arity)
        if (callsSinceCheck >= CHECK_EVERY) {
            callsSinceCheck = 0
            val environment = environment ?: ExecutionEnvironment(startTime = 0)
            if (currentTimeInstant() > deadline) {
                val context: ExecutionContext = PrologExecutionContext(environment)
                throw Halt(TimeOutException(exceededDuration = environment.maxDuration, context = context))
            }
            if (resolutionInterrupted()) {
                val context: ExecutionContext = PrologExecutionContext(environment)
                throw Halt(ResolutionException(INTERRUPTED, context = context))
            }
        }
    }

    private fun callBuiltin(
        builtin: Builtin,
        functor: Int,
        arity: Int,
    ): Int {
        // (the clauses the theory of the user gives a builtin are its alternatives: the strict rules of an
        // argumentation theory are clauses of ->/2)
        if (theoryVisible) {
            program.theory.predicate(functor)?.let { pushChoicePoint(functor, 0, arity, cpTop, continuation, 0, THEORY) }
        }
        return builtin.call(this)
    }

    private fun callPrimitiveOf(
        functor: Int,
        arity: Int,
    ): Int {
        val primitive = scope.primitive(functor) ?: return FAILED
        return callPrimitive(primitive, functor, arity)
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
            REF -> throw Raised(error(Atom.of("instantiation_error"), "call/1"))
            else -> throw Raised(error(Struct.of("type_error", Atom.of("callable"), toTerm(g, HashMap())), "call/1"))
        }
        return call(functor, arity)
    }

    // ---- the instructions

    /**
     * Runs the code from [pc] until the continuation of the run reaches its end (true) or an instruction fails
     * (false); the program counter lives in a local meanwhile, written to [pc] around the calls
     */
    fun execute(): Boolean {
        var code = program.code
        var ip = pc
        while (true) {
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
                    if (!unify(regs[code[at + 1]], regs[code[at + 2]])) return false
                    ip = at + 3
                }
                Op.GET_Y_VAL -> {
                    if (!unify(local[frame + FRAME + code[at + 1]], regs[code[at + 2]])) return false
                    ip = at + 3
                }
                Op.GET_CONST -> {
                    if (!unifyConstant(code[at + 1], regs[code[at + 2]])) return false
                    ip = at + 3
                }
                Op.GET_STR -> {
                    if (!getStructure(code[at + 1], regs[code[at + 2]])) return false
                    ip = at + 3
                }
                Op.GET_LIST -> {
                    if (!getList(regs[code[at + 1]])) return false
                    ip = at + 2
                }
                Op.GET_LIST_VV -> {
                    val d = deref(regs[code[at + 1]])
                    when (tag(d)) {
                        REF -> {
                            val h = alloc(2)
                            heap[h] = ref(h)
                            varInfo[h] = code[at + 3]
                            heap[h + 1] = ref(h + 1)
                            varInfo[h + 1] = code[at + 5]
                            regs[code[at + 2]] = ref(h)
                            regs[code[at + 4]] = ref(h + 1)
                            bind(payload(d), word(LIST, h))
                        }
                        LIST -> {
                            val a = payload(d)
                            regs[code[at + 2]] = heap[a]
                            regs[code[at + 4]] = heap[a + 1]
                        }
                        else -> return false
                    }
                    ip = at + 6
                }
                Op.GET_LIST2 -> {
                    if (!getList(regs[code[at + 1]])) return false
                    if (!unifySub(code[at + 2], code[at + 3], code[at + 4])) return false
                    if (!unifySub(code[at + 5], code[at + 6], code[at + 7])) return false
                    ip = at + 8
                }
                Op.GET_STR2 -> {
                    if (!getStructure(code[at + 1], regs[code[at + 2]])) return false
                    if (!unifySub(code[at + 3], code[at + 4], code[at + 5])) return false
                    if (!unifySub(code[at + 6], code[at + 7], code[at + 8])) return false
                    ip = at + 9
                }
                Op.UNIFY_X_VAR -> {
                    regs[code[at + 1]] = if (writing) ref(pushVariable(code[at + 2])) else heap[next++]
                    ip = at + 3
                }
                Op.UNIFY_Y_VAR -> {
                    local[frame + FRAME + code[at + 1]] = if (writing) ref(pushVariable(code[at + 2])) else heap[next++]
                    ip = at + 3
                }
                Op.UNIFY_X_VAL -> {
                    if (writing) {
                        pushCell(regs[code[at + 1]])
                    } else if (!unify(regs[code[at + 1]], heap[next++])) {
                        return false
                    }
                    ip = at + 2
                }
                Op.UNIFY_Y_VAL -> {
                    if (writing) {
                        pushCell(local[frame + FRAME + code[at + 1]])
                    } else if (!unify(local[frame + FRAME + code[at + 1]], heap[next++])) {
                        return false
                    }
                    ip = at + 2
                }
                Op.UNIFY_CONST -> {
                    if (writing) {
                        pushCell(code[at + 1])
                    } else if (!unifyConstant(code[at + 1], heap[next++])) {
                        return false
                    }
                    ip = at + 2
                }
                Op.UNIFY_VOID -> {
                    val n = code[at + 1]
                    if (writing) pushVoids(n) else next += n
                    ip = at + 2
                }
                Op.PUT_X_VAR -> {
                    val h = newVariable(code[at + 3])
                    regs[code[at + 1]] = ref(h)
                    regs[code[at + 2]] = ref(h)
                    ip = at + 4
                }
                Op.PUT_Y_VAR -> {
                    val h = newVariable(code[at + 3])
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
                Op.PUT_STR -> {
                    val h = alloc(1)
                    heap[h] = code[at + 1]
                    regs[code[at + 2]] = word(STR, h)
                    ip = at + 3
                }
                Op.PUT_LIST -> {
                    regs[code[at + 1]] = word(LIST, top)
                    ip = at + 2
                }
                Op.SET_X_VAR -> {
                    regs[code[at + 1]] = ref(pushVariable(code[at + 2]))
                    ip = at + 3
                }
                Op.SET_Y_VAR -> {
                    local[frame + FRAME + code[at + 1]] = ref(pushVariable(code[at + 2]))
                    ip = at + 3
                }
                Op.SET_X_VAL -> {
                    pushCell(regs[code[at + 1]])
                    ip = at + 2
                }
                Op.SET_Y_VAL -> {
                    pushCell(local[frame + FRAME + code[at + 1]])
                    ip = at + 2
                }
                Op.SET_CONST -> {
                    pushCell(code[at + 1])
                    ip = at + 2
                }
                Op.SET_VOID -> {
                    pushVoids(code[at + 1])
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
                    ip = at + 3
                    continuation = ip
                    cutBarrier = cpTop
                    pc = ip
                    when (call(code[at + 1], code[at + 2])) {
                        FAILED -> return false
                        JUMPED -> ip = pc
                        else -> Unit
                    }
                    code = program.code
                }
                Op.EXECUTE -> {
                    cutBarrier = cpTop
                    when (call(code[at + 1], code[at + 2])) {
                        FAILED -> return false
                        CONTINUE -> ip = continuation
                        else -> ip = pc
                    }
                    code = program.code
                }
                Op.DEALLOCATE_EXECUTE -> {
                    deallocate()
                    cutBarrier = cpTop
                    when (call(code[at + 1], code[at + 2])) {
                        FAILED -> return false
                        CONTINUE -> ip = continuation
                        else -> ip = pc
                    }
                    code = program.code
                }
                Op.RETURN -> {
                    deallocate()
                    ip = continuation
                }
                Op.PROCEED -> ip = continuation
                Op.CUT -> {
                    cutTo(local[frame + 2])
                    ip = at + 1
                }
                Op.GET_LEVEL -> {
                    local[frame + FRAME + code[at + 1]] = Words.int(cpTop)
                    ip = at + 2
                }
                Op.CUT_Y -> {
                    cutTo(payload(local[frame + FRAME + code[at + 1]]))
                    ip = at + 2
                }
                Op.CUT_LOCAL -> {
                    cutTo(payload(local[frame + FRAME + code[at + 1]]) + 1)
                    ip = at + 2
                }
                Op.TRY_ELSE -> {
                    pushChoicePoint(RESUME, 0, 0, 0, code[at + 1])
                    ip = at + 2
                }
                Op.TRUST_ME -> ip = at + 1
                Op.JUMP -> ip = code[at + 1]
                Op.FAIL -> return false
                Op.CALL_TERM -> {
                    val goal = local[frame + FRAME + code[at + 1]]
                    ip = at + 3
                    continuation = ip
                    cutBarrier = if (code[at + 2] == Op.TRANSPARENT) local[frame + 2] else cpTop
                    pc = ip
                    when (callTerm(goal)) {
                        FAILED -> return false
                        JUMPED -> ip = pc
                        else -> Unit
                    }
                    code = program.code
                }
                Op.EXECUTE_TERM -> {
                    val goal = regs[code[at + 1]]
                    if (code[at + 2] == Op.OPAQUE) cutBarrier = cpTop
                    when (callTerm(goal)) {
                        FAILED -> return false
                        CONTINUE -> ip = continuation
                        else -> ip = pc
                    }
                    code = program.code
                }
                Op.STOP -> {
                    pc = at
                    return true
                }
                Op.ARITH_IS -> {
                    val n = code[at + 3]
                    ip = at + 4 + n
                    val value = evaluate(code, at + 4, n)
                    if (!evalFailed) {
                        val w = program.intWord(value)
                        val operand = code[at + 2]
                        when (code[at + 1]) {
                            Op.T_X -> if (!unify(regs[operand], w)) return false
                            Op.T_Y -> if (!unify(local[frame + FRAME + operand], w)) return false
                            Op.T_NEW_X -> regs[operand] = w
                            Op.T_NEW_Y -> local[frame + FRAME + operand] = w
                            else -> if (operand != w) return false
                        }
                    } else {
                        // The builtin: the target and the expression as words in the registers, which are kept
                        val a0 = regs[0]
                        val a1 = regs[1]
                        regs[0] = targetWord(code[at + 1], code[at + 2])
                        regs[1] = build(code, at + 4, n)
                        continuation = ip
                        cutBarrier = cpTop
                        pc = ip
                        when (call(fIs, 2)) {
                            FAILED -> return false
                            JUMPED -> ip = pc
                            else -> Unit
                        }
                        regs[0] = a0
                        regs[1] = a1
                        code = program.code
                    }
                }
                Op.ARITH_CMP -> {
                    val nA = code[at + 2]
                    val nB = code[at + 3 + nA]
                    ip = at + 4 + nA + nB
                    val a = evaluate(code, at + 3, nA)
                    val b = if (evalFailed) 0L else evaluate(code, at + 4 + nA, nB)
                    if (!evalFailed) {
                        val holds =
                            when (code[at + 1]) {
                                Op.C_LT -> a < b
                                Op.C_GT -> a > b
                                Op.C_LE -> a <= b
                                Op.C_GE -> a >= b
                                Op.C_EQ -> a == b
                                else -> a != b
                            }
                        if (!holds) return false
                    } else {
                        val a0 = regs[0]
                        val a1 = regs[1]
                        regs[0] = build(code, at + 3, nA)
                        regs[1] = build(code, at + 4 + nA, nB)
                        continuation = ip
                        cutBarrier = cpTop
                        pc = ip
                        when (call(comparisonFunctors[code[at + 1]], 2)) {
                            FAILED -> return false
                            JUMPED -> ip = pc
                            else -> Unit
                        }
                        regs[0] = a0
                        regs[1] = a1
                        code = program.code
                    }
                }
                Op.INIT_Y -> {
                    local[frame + FRAME + code[at + 1]] = ref(newVariable(code[at + 2]))
                    ip = at + 3
                }
                Op.RESTORE_SCOPE -> {
                    scope = program.scope(payload(local[frame + FRAME]))
                    theoryVisible = payload(local[frame + FRAME + 1]) != 0
                    ip = at + 1
                }
                else -> throw IllegalStateException("unknown instruction ${code[at]} at $at")
            }
        }
    }

    // ---- inline arithmetic

    // The value of the expression code at [from] ([n] words): [evalFailed] when an operand is not an integer (the
    // builtin then raises the error, or computes it)
    private fun evaluate(
        code: IntArray,
        from: Int,
        n: Int,
    ): Long {
        evalFailed = false
        var sp = 0
        var i = from
        val end = from + n
        while (i < end) {
            val kind = code[i]
            val operand = code[i + 1]
            i += 2
            when (kind) {
                Op.E_X, Op.E_Y -> {
                    val w = deref(if (kind == Op.E_X) regs[operand] else local[frame + FRAME + operand])
                    val v =
                        when (tag(w)) {
                            INT -> payload(w).toLong()
                            BIG -> program.bigs[payload(w)]
                            else -> {
                                evalFailed = true
                                return 0
                            }
                        }
                    if (sp == values.size) values = values.copyOf(sp * 2)
                    values[sp++] = v
                }
                Op.E_CONST -> {
                    if (sp == values.size) values = values.copyOf(sp * 2)
                    values[sp++] = payload(operand).toLong()
                }
                Op.E_ADD -> {
                    sp--
                    values[sp - 1] = values[sp - 1] + values[sp]
                }
                Op.E_SUB -> {
                    sp--
                    values[sp - 1] = values[sp - 1] - values[sp]
                }
                Op.E_MUL -> {
                    sp--
                    values[sp - 1] = values[sp - 1] * values[sp]
                }
                Op.E_IDIV -> {
                    sp--
                    val divisor = values[sp]
                    if (divisor == 0L) {
                        evalFailed = true
                        return 0
                    }
                    values[sp - 1] = values[sp - 1] / divisor
                }
                Op.E_MOD -> {
                    sp--
                    val divisor = values[sp]
                    if (divisor == 0L) {
                        evalFailed = true
                        return 0
                    }
                    values[sp - 1] = values[sp - 1].mod(divisor)
                }
                else -> values[sp - 1] = -values[sp - 1]
            }
        }
        return values[0]
    }

    // The expression code at [from] as a term on the heap, for the builtin
    private fun build(
        code: IntArray,
        from: Int,
        n: Int,
    ): Int {
        var sp = 0
        var i = from
        val end = from + n
        while (i < end) {
            val kind = code[i]
            val operand = code[i + 1]
            i += 2
            when (kind) {
                Op.E_X, Op.E_Y, Op.E_CONST -> {
                    if (sp == exprWords.size) exprWords = exprWords.copyOf(sp * 2)
                    exprWords[sp++] =
                        when (kind) {
                            Op.E_X -> regs[operand]
                            Op.E_Y -> local[frame + FRAME + operand]
                            else -> operand
                        }
                }
                Op.E_NEG -> {
                    val h = alloc(2)
                    heap[h] = wNeg
                    heap[h + 1] = exprWords[sp - 1]
                    exprWords[sp - 1] = word(STR, h)
                }
                else -> {
                    val h = alloc(3)
                    heap[h] =
                        when (kind) {
                            Op.E_ADD -> wPlus
                            Op.E_SUB -> wMinus
                            Op.E_MUL -> wTimes
                            Op.E_IDIV -> wIntDiv
                            else -> wMod
                        }
                    sp--
                    heap[h + 1] = exprWords[sp - 1]
                    heap[h + 2] = exprWords[sp]
                    exprWords[sp - 1] = word(STR, h)
                }
            }
        }
        return exprWords[0]
    }

    // The target of an ARITH_IS as a word (a first occurrence created where it lives)
    private fun targetWord(
        kind: Int,
        operand: Int,
    ): Int =
        when (kind) {
            Op.T_X -> regs[operand]
            Op.T_Y -> local[frame + FRAME + operand]
            Op.T_NEW_X -> ref(newVariable()).also { regs[operand] = it }
            Op.T_NEW_Y -> ref(newVariable()).also { local[frame + FRAME + operand] = it }
            else -> operand
        }

    // The structure [f] against [w]: entered in read mode, or built in write mode
    private fun getStructure(
        f: Int,
        w: Int,
    ): Boolean {
        val d = deref(w)
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
        return true
    }

    private fun getList(w: Int): Boolean {
        val d = deref(w)
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
        return true
    }

    // A sub-instruction of GET_LIST2 and GET_STR2 (see Op): one argument of the structure entered
    private fun unifySub(
        kind: Int,
        a: Int,
        b: Int,
    ): Boolean {
        when (kind) {
            Op.U_X_VAR -> regs[a] = if (writing) ref(pushVariable(b)) else heap[next++]
            Op.U_Y_VAR -> local[frame + FRAME + a] = if (writing) ref(pushVariable(b)) else heap[next++]
            Op.U_X_VAL ->
                if (writing) {
                    pushCell(regs[a])
                } else if (!unify(regs[a], heap[next++])) {
                    return false
                }
            Op.U_Y_VAL ->
                if (writing) {
                    pushCell(local[frame + FRAME + a])
                } else if (!unify(local[frame + FRAME + a], heap[next++])) {
                    return false
                }
            Op.U_CONST ->
                if (writing) {
                    pushCell(a)
                } else if (!unifyConstant(a, heap[next++])) {
                    return false
                }
            else -> if (writing) pushVoids(1) else next++
        }
        return true
    }

    private fun pushVoids(n: Int) {
        val h = alloc(n)
        for (i in h until h + n) {
            heap[i] = ref(i)
            varInfo[i] = 0
        }
    }

    /**
     * Runs until a run reaches its end (true: a solution) or no choice point above [cpBase] is left; a ball no
     * catch/3 above [cpBase] takes is thrown on
     */
    fun run(cpBase: Int): Boolean {
        while (true) {
            val ok =
                try {
                    execute()
                } catch (raised: Raised) {
                    when (unwind(raised, cpBase)) {
                        UNCAUGHT -> throw raised
                        EXHAUSTED -> return false
                        else -> continue
                    }
                }
            if (ok) return true
            if (!backtrack(cpBase)) return false
        }
    }

    // ---- exceptions

    /** The ball `error(Type, Context)` of a standard error raised by the builtin [where] */
    private fun error(
        type: Term,
        where: String,
    ): Term = Struct.of("error", type, Atom.of(where))

    // catch(Goal, Catcher, Recovery): a choice point the unwinding of a ball stops at, then the goal
    private fun catch(): Int {
        pushChoicePoint(CATCH, 0, 3, 0, continuation)
        cutBarrier = cpTop
        return callTerm(regs[0])
    }

    // Unwinds the choice points above [cpBase] to the nearest catch/3 whose catcher unifies with the ball of
    // [raised], and runs its recovery: UNCAUGHT when there is none, EXHAUSTED when the recovery fails with no choice
    // point left above [cpBase], RESUMED otherwise
    private fun unwind(
        raised: Raised,
        cpBase: Int,
    ): Int {
        while (cpTop > cpBase) {
            val base = (cpTop - 1) * CP_SIZE
            if (cps[base + 6] != CATCH) {
                popChoicePoint()
                continue
            }
            restoreTop()
            val argsBase = cps[base + 9]
            val catcher = cpArgs[argsBase + 1]
            val recovery = cpArgs[argsBase + 2]
            popChoicePoint()
            val ball = fromTerm(raised.ball, HashMap())
            if (unify(catcher, ball)) {
                cutBarrier = cpTop
                when (callTerm(recovery)) {
                    FAILED -> if (!backtrack(cpBase)) return EXHAUSTED
                    CONTINUE -> pc = continuation
                    else -> Unit
                }
                return RESUMED
            }
        }
        return UNCAUGHT
    }

    /** The resolution exception of a ball no catch took */
    fun haltOf(raised: Raised): ResolutionException {
        raised.exception?.let { return it }
        val context: ExecutionContext = PrologExecutionContext(environment ?: ExecutionEnvironment(startTime = 0))
        val ball = raised.ball
        return if (ball is Struct && ball.functor == "error" && ball.arity in 1..2 && ball[0] is Struct) {
            try {
                LogicError.of(context = context, type = ball[0] as Struct, extraData = if (ball.arity > 1) ball[1] else null)
            } catch (_: IllegalArgumentException) {
                MessageError.of(ball, context)
            }
        } else {
            MessageError.of(ball, context)
        }
    }

    // The ball of a resolution exception, as catch/3 sees it
    private fun ballOf(exception: ResolutionException): Term =
        when (exception) {
            is MessageError -> exception.content
            is LogicError -> exception.errorStruct
            else -> throw Halt(exception)
        }

    // ---- arithmetic

    /**
     * The value of the integer expression [w0]: a small or large integer, or the four operations, integer division,
     * modulo and negation of such; anything else (a float, another function, an error) is left to the library
     * ([NotNative])
     */
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
                    fIntDiv -> {
                        val divisor = eval(heap[a + 2])
                        if (divisor == 0L) throw NotNative()
                        eval(heap[a + 1]) / divisor
                    }
                    fMod -> {
                        val divisor = eval(heap[a + 2])
                        if (divisor == 0L) throw NotNative()
                        eval(heap[a + 1]).mod(divisor)
                    }
                    fNeg -> -eval(heap[a + 1])
                    else -> throw NotNative()
                }
            }
            else -> throw NotNative()
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
        // The barrier choice point: the state before the goal, resumed at the continuation of the call
        pushChoicePoint(BARRIER, 0, 0, 0, continuation)
        val cpBase = cpTop
        continuation = stopAddress
        cutBarrier = cpTop
        var found =
            nested(cpBase) {
                when (callTerm(goal)) {
                    FAILED -> backtrack(cpBase) && run(cpBase)
                    CONTINUE -> {
                        pc = continuation
                        run(cpBase)
                    }
                    else -> run(cpBase)
                }
            }
        var bag = IntArray(64)
        var bagInfo = IntArray(64)
        var bagTop = 0
        var roots = IntArray(16)
        var count = 0
        while (found) {
            if (count == roots.size) roots = roots.copyOf(count * 2)
            val copier = Copier(bag, bagTop, bagInfo)
            val root = copier.copy(deref(pinnedWords[pinnedTemplate]))
            bag = copier.bag
            bagInfo = copier.bagInfo
            bagTop = copier.top
            roots[count++] = root
            found = nested(cpBase) { backtrack(cpBase) && run(cpBase) }
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
            varInfo[base + i] = bagInfo[i]
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

    // A step of a nested run (above [cpBase]): a ball thrown by it, outside the run loop (by the call starting it, by
    // the backtracking resuming it), is unwound within the run as the loop would
    private inline fun nested(
        cpBase: Int,
        step: () -> Boolean,
    ): Boolean =
        try {
            step()
        } catch (raised: Raised) {
            when (unwind(raised, cpBase)) {
                UNCAUGHT -> throw raised
                EXHAUSTED -> false
                else -> run(cpBase)
            }
        }

    // A copy of a term of the heap into a bag of its own (relative addresses), its unbound variables fresh
    private inner class Copier(
        var bag: IntArray,
        var top: Int,
        var bagInfo: IntArray,
    ) {
        private val variables = HashMap<Int, Int>()

        private fun allocBag(cells: Int): Int {
            if (top + cells > bag.size) {
                bag = bag.copyOf(maxOf(bag.size * 2, top + cells))
                bagInfo = bagInfo.copyOf(bag.size)
            }
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
                            bagInfo[a] = varInfo[payload(w)]
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

    // Calls [primitive] on the registers: the goal as terms, the first answer unified back, the others left as a
    // choice point resuming at the continuation (the next instruction of a call, see Op.CALL); a delegated call
    // is run by the machine itself
    private fun callPrimitive(
        primitive: Primitive,
        functor: Int,
        arity: Int,
    ): Int {
        if (primitive is NativePrimitive) return callNative(primitive, functor, arity)
        val names = HashMap<Int, Var>()
        val arguments = List(arity) { toTerm(regs[it], names) }
        val name = program.nameOfFunctor(functor)
        val goal = if (arity == 0) Atom.of(name) else Struct.of(name, arguments)
        val context: ExecutionContext = PrologExecutionContext(environmentSeen(), procedure = goal)
        val request = Solve.Request(Signature(name, arity), arguments, context)
        if (primitive is DelegatedCall) {
            val delegation =
                try {
                    primitive.delegate(request)
                } catch (exception: Exception) {
                    throw raised(exception)
                }
            if (delegation != null) {
                val cells = HashMap<Var, Int>(names.size * 2)
                for ((cell, variable) in names) cells[variable] = cell
                return when (delegation) {
                    is Inlining -> inline(delegation, cells)
                    is Sourcing -> sourced(delegation, cells)
                }
            }
        }
        val responses =
            try {
                primitive.solve(request).iterator()
            } catch (exception: Exception) {
                throw raised(exception)
            }
        val any =
            try {
                responses.hasNext()
            } catch (exception: Exception) {
                throw raised(exception)
            }
        if (!any) return FAILED
        val response = nextResponse(responses)
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
        if (hasNextResponse(call)) {
            pushChoicePoint(PRIMITIVE, calls.size, 0, 0, continuation)
            calls.add(call)
        }
        return if (applyResponse(response, call)) CONTINUE else FAILED
    }

    // Whether [call] has an answer after the current one: computed ahead (as the engine does, to know whether the
    // call leaves alternatives), an error computing it kept for when the resolution backtracks into it
    private fun hasNextResponse(call: PrimitiveCall): Boolean =
        try {
            call.responses.hasNext()
        } catch (exception: Exception) {
            call.pending = exception
            true
        }

    private fun nextResponse(responses: Iterator<Solve.Response>): Solve.Response =
        try {
            responses.next()
        } catch (exception: Exception) {
            throw raised(exception)
        }

    // The ball of an exception the code of a primitive raised: a resolution exception as it is, any other a
    // system_error catch/3 can handle (as the engine does)
    // ---- native primitives (the machine as their NativeCall)

    private fun callNative(
        primitive: NativePrimitive,
        functor: Int,
        arity: Int,
    ): Int {
        nativeFunctor = functor
        nativeArity = arity
        nativeNames = null
        return try {
            primitive.call(this)
        } catch (raised: Raised) {
            throw raised
        } catch (halt: Halt) {
            throw halt
        } catch (exception: Exception) {
            throw raised(exception)
        }
    }

    override val arity: Int get() = nativeArity

    override fun arg(i: Int): Int = deref(regs[i])

    override fun kind(w: Int): Int =
        when (tag(w)) {
            REF -> NativeCall.VAR
            ATOM -> NativeCall.ATOM
            INT, BIG -> NativeCall.INT
            FLT -> NativeCall.FLOAT
            STR -> NativeCall.STRUCT
            else -> NativeCall.LIST
        }

    override fun atomName(w: Int): String = program.atoms[payload(w)]

    override fun intValue(w: Int): Long = if (tag(w) == INT) payload(w).toLong() else program.bigs[payload(w)]

    override fun functorName(w: Int): String = if (tag(w) == LIST) "." else program.functorName(heap[payload(w)])

    override fun arityOf(w: Int): Int = if (tag(w) == LIST) 2 else program.arity(heap[payload(w)])

    override fun argOf(
        w: Int,
        i: Int,
    ): Int = deref(heap[payload(w) + (if (tag(w) == LIST) i else i + 1)])

    override fun atom(name: String): Int = program.atomWord(name)

    override fun int(value: Long): Int = program.intWord(value)

    override fun struct(
        name: String,
        args: IntArray,
    ): Int {
        if (args.isEmpty()) return program.atomWord(name)
        val h = alloc(args.size + 1)
        heap[h] = program.functorWord(name, args.size)
        for (i in args.indices) heap[h + 1 + i] = args[i]
        return word(STR, h)
    }

    override fun list(
        items: IntArray,
        tail: Int,
    ): Int {
        var list = tail
        for (k in items.indices.reversed()) {
            val cell = alloc(2)
            heap[cell] = items[k]
            heap[cell + 1] = list
            list = word(LIST, cell)
        }
        return list
    }

    override val emptyList: Int get() = program.emptyList

    override fun variable(): Int = ref(newVariable())

    override fun term(w: Int): Term = toTerm(w, nativeNames ?: HashMap<Int, Var>().also { nativeNames = it })

    override fun word(term: Term): Int {
        val names = nativeNames
        val cells = HashMap<Var, Int>()
        if (names != null) for ((cell, variable) in names) cells[variable] = cell
        return fromTerm(term, cells)
    }

    override val context: ExecutionContext get() = PrologExecutionContext(environmentSeen())

    override val libraries: Runtime get() = scope.libraries ?: environment!!.libraries

    override val rootLibraries: Runtime get() = environment!!.libraries

    override fun sourced(
        goal: Int,
        source: ClauseSource,
    ): Int {
        val g = deref(goal)
        val goalTerm = term(g).castToStruct()
        val clauses = source.clausesFor(goalTerm, environment!!.unificator)
        if (clauses.isEmpty()) return FAILED
        // The arguments of the goal looked up in the registers (those of the call were the primitive's)
        val goalArity = goalTerm.arity
        if (goalArity > regs.size) regs = regs.copyOf(goalArity * 2)
        if (tag(g) == STR) {
            val a = payload(g)
            for (i in 0 until goalArity) regs[i] = heap[a + 1 + i]
        } else if (tag(g) == LIST) {
            val a = payload(g)
            regs[0] = heap[a]
            regs[1] = heap[a + 1]
        }
        val call = SourcedCall(clauses)
        val index = calls.size
        calls.add(call)
        return trySourced(call, 0, goalArity, index)
    }

    override fun inline(
        goal: Int,
        libraries: Runtime,
        theoryVisible: Boolean,
    ): Int = inlineWord(goal, libraries, theoryVisible)

    override fun alternatives(resume: NativeResume) {
        pushChoicePoint(NATIVE, calls.size, nativeArity, 0, continuation)
        calls.add(resume)
    }

    private fun raised(exception: Exception): Raised {
        val resolution =
            exception as? ResolutionException
                ?: SystemError.forUncaughtException(PrologExecutionContext(environment ?: ExecutionEnvironment(startTime = 0)), exception)
        return Raised(ballOf(resolution), resolution)
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
            is Solution.Halt -> throw Raised(ballOf(solution.exception), solution.exception)
        }
    }

    private fun applySideEffects(sideEffects: List<SideEffect>) {
        var environment = environment!!
        for (effect in sideEffects) {
            when (effect) {
                is SideEffect.AddDynamicClauses -> {
                    for (clause in effect.clauses) program.theory.add(clause, dynamic = true, onTop = effect.onTop)
                    prepare()
                }
                is SideEffect.RemoveDynamicClauses -> for (clause in effect.clauses) program.theory.remove(clause)
                is SideEffect.ResetDynamicKb -> throw IllegalStateException("side effect not supported by the machine: $effect")
                else -> Unit
            }
            // (the knowledge bases, flags, libraries, operators and channels of the environment, as the effect
            // leaves them: a mutable theory is changed in place)
            environment = effect.applyTo(PrologExecutionContext(environment)).let { it as PrologExecutionContext }.environment
        }
        this.environment = environment
    }

    // ---- delegated calls: module calls and lookups

    // The goal of [inlining] run with its libraries, in a frame of its own holding the scope to go back to
    private fun inline(
        inlining: Inlining,
        cells: HashMap<Var, Int>,
    ): Int = inlineWord(fromTerm(inlining.goal, cells), inlining.libraries, inlining.theory)

    // The goal [goal] entered against [libraries] (the theory of the user visible or not), the scope of the caller
    // restored by the frame allocated here when the goal returns
    private fun inlineWord(
        goal: Int,
        libraries: Runtime,
        theory: Boolean,
    ): Int {
        allocate(2)
        local[frame + FRAME] = Words.int(scope.id)
        local[frame + FRAME + 1] = Words.int(if (theoryVisible) 1 else 0)
        continuation = restoreScopeBlock
        scope = program.scopeOf(libraries)
        if (program.maxRegister + 1 > regs.size) regs = regs.copyOf(program.maxRegister + 1)
        theoryVisible = theory
        cutBarrier = cpTop
        return when (callTerm(goal)) {
            FAILED -> FAILED
            CONTINUE -> jump(continuation)
            else -> JUMPED
        }
    }

    // The goal of [sourcing] resolved with the clauses its source gives for it
    private fun sourced(
        sourcing: Sourcing,
        cells: HashMap<Var, Int>,
    ): Int {
        val clauses = sourcing.source.clausesFor(sourcing.goal, environment!!.unificator)
        if (clauses.isEmpty()) return FAILED
        // The arguments of the goal looked up in the registers (those of the call were the primitive's)
        val goal = sourcing.goal
        val goalArity = goal.arity
        if (goalArity > regs.size) regs = regs.copyOf(goalArity * 2)
        for (i in 0 until goalArity) regs[i] = fromTerm(goal[i], cells)
        val call = SourcedCall(clauses)
        val index = calls.size
        calls.add(call)
        return trySourced(call, 0, goalArity, index)
    }

    // Tries the clauses of [call] from [from] on the goal in the registers: its head unified with the arguments,
    // its body called; a choice point is left while clauses remain
    private fun trySourced(
        call: SourcedCall,
        from: Int,
        arity: Int,
        index: Int,
    ): Int {
        val clauses = call.clauses
        if (from == clauses.size) return FAILED
        if (from + 1 < clauses.size) pushChoicePoint(SOURCED, index, arity, 0, continuation, from + 1)
        val clause = clauses[from]
        val variables = HashMap<Var, Int>()
        val head = clause.head!!
        for (i in 0 until arity) if (!unify(fromTerm(head[i], variables), regs[i])) return FAILED
        if (clause.body.isTrue) return CONTINUE
        cutBarrier = cpTop
        return callTerm(fromTerm(clause.body, variables))
    }

    // ---- garbage collection

    /** Collects the heap cells no longer reachable, [arity] registers holding the arguments of the call at hand */
    fun collect(arity: Int) {
        collections++
        collectedFrom = top
        val blocks = (top + 63) ushr 6
        if (marks.size < blocks) marks = LongArray(maxOf(blocks, (heap.size + 63) ushr 6))
        marks.fill(0L, 0, blocks)
        if (counts.size <= blocks) counts = IntArray(maxOf(blocks + 1, counts.size * 2))
        // Marking: the cells reachable from the roots
        for (i in 0 until arity) markWord(regs[i])
        if (frameVisited.size < local.size) frameVisited = BooleanArray(local.size)
        frameVisited.fill(false, 0, local.size)
        markFrames(frame)
        for (cp in 0 until cpTop) markFrames(cps[cp * CP_SIZE + 3])
        for (i in 0 until cpArgsTop) markWord(cpArgs[i])
        for (i in 0 until trailTop) markCell(trail[i])
        for (i in 0 until pinnedWordsTop) markWord(pinnedWords[i])
        for (call in calls) if (call is PrimitiveCall) for (c in call.cells) markCell(c)
        // The new addresses: the cells kept below each block
        var n = 0
        for (b in 0 until blocks) {
            counts[b] = n
            n += marks[b].countOneBits()
        }
        counts[blocks] = n
        collectedTo = n
        // The roots relocated
        for (i in 0 until arity) regs[i] = relocated(regs[i])
        frameVisited.fill(false, 0, local.size)
        relocateFrames(frame)
        for (cp in 0 until cpTop) relocateFrames(cps[cp * CP_SIZE + 3])
        for (i in 0 until cpArgsTop) cpArgs[i] = relocated(cpArgs[i])
        for (i in 0 until trailTop) trail[i] = newAddress(trail[i])
        for (i in 0 until pinnedWordsTop) pinnedWords[i] = relocated(pinnedWords[i])
        for (call in calls) if (call is PrimitiveCall) for (i in call.cells.indices) call.cells[i] = newAddress(call.cells[i])
        for (cp in 0 until cpTop) {
            val base = cp * CP_SIZE
            cps[base + 1] = newAddress(cps[base + 1])
        }
        // The cells kept slide down, their words relocated, their names with them
        var f = 0
        for (b in 0 until blocks) {
            var bits = marks[b]
            val base = b shl 6
            while (bits != 0L) {
                val i = base + bits.countTrailingZeroBits()
                heap[f] = relocated(heap[i])
                varInfo[f] = varInfo[i]
                f++
                bits = bits and (bits - 1)
            }
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

    private fun marked(c: Int): Boolean = (marks[c ushr 6] ushr (c and 63)) and 1L != 0L

    private fun mark(c: Int) {
        marks[c ushr 6] = marks[c ushr 6] or (1L shl (c and 63))
    }

    // The new address of a cell kept, or of a mark [address] (the first cell at or after it kept): the cells kept
    // below it
    private fun newAddress(address: Int): Int {
        if (address >= collectedFrom) return collectedTo
        val b = address ushr 6
        return counts[b] + (marks[b] and ((1L shl (address and 63)) - 1)).countOneBits()
    }

    private fun relocated(w: Int): Int = if (tag(w) == REF || tag(w) == STR || tag(w) == LIST) word(tag(w), newAddress(payload(w))) else w

    private fun markCell(c: Int) {
        if (marked(c)) return
        mark(c)
        var sp = 0
        markStack[sp++] = c
        while (sp > 0) {
            val cell = markStack[--sp]
            val w = heap[cell]
            when (tag(w)) {
                REF -> {
                    val a = payload(w)
                    if (!marked(a)) {
                        mark(a)
                        sp = push(sp, a)
                    }
                }
                STR -> {
                    val a = payload(w)
                    if (!marked(a)) {
                        mark(a)
                        val n = program.arity(heap[a])
                        for (i in 1..n) {
                            if (!marked(a + i)) {
                                mark(a + i)
                                sp = push(sp, a + i)
                            }
                        }
                    }
                }
                LIST -> {
                    val a = payload(w)
                    for (i in 0..1) {
                        if (!marked(a + i)) {
                            mark(a + i)
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
                if (!marked(a)) {
                    mark(a)
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
            is Var ->
                ref(
                    variables.getOrPut(term) {
                        varObjects.add(term)
                        newVariable(-varObjects.size)
                    },
                )
            is Integer -> program.intWord(term.value.toLong())
            is Real -> program.floatWord(term)
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
            REF -> names.getOrPut(payload(w)) { variableOf(payload(w)) }
            ATOM -> Atom.of(program.atoms[payload(w)])
            INT -> Integer.of(payload(w))
            BIG -> Integer.of(program.bigs[payload(w)])
            FLT -> program.floats[payload(w)]
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

    // The variable a cell holding an unbound one is read as (see varInfo)
    private fun variableOf(cell: Int): Var {
        val info = varInfo[cell]
        return when {
            info > 0 -> Var.of(program.atoms[info - 1])
            info < 0 -> varObjects[-info - 1]
            else -> Var.of("_G$cell")
        }
    }

    /** The kinds of the choice points left, oldest first: a diagnostic */
    fun choicePointKinds(): List<String> =
        (0 until cpTop).map { cp ->
            when (val functor = cps[cp * CP_SIZE + 6]) {
                BARRIER -> "barrier"
                RESUME -> "resume"
                PRIMITIVE -> "primitive"
                CATCH -> "catch"
                SOURCED -> "sourced"
                NATIVE -> "native"
                else -> "clauses of ${program.nameOfFunctor(functor)}/${program.functorArities[functor]} from ${cps[cp * CP_SIZE + 7]}"
            }
        }

    /** The machine emptied: a top-level query starts from nothing */
    fun reset() {
        varObjects.clear()
        top = 0
        heapBarrier = 0
        trailTop = 0
        frame = NONE
        cpTop = 0
        cpArgsTop = 0
        pinnedWordsTop = 0
        calls.clear()
        theoryVisible = initialTheoryVisible
        callsSinceCheck = 0
        scope = environment?.let { program.scopeOf(it.libraries) } ?: program.theory
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
            prepare()
            // (the cells of the variables of the query, as the arguments of its code, pinned: the collector moves them)
            val pins = LinkedHashMap<Var, Int>()
            queryCells = compiled.variables.size
            if (queryBound.size < queryCells) queryBound = IntArray(queryCells)
            queryBound.fill(0, 0, queryCells)
            bindings = 0
            for ((i, variable) in compiled.variables.withIndex()) {
                varObjects.add(variable)
                val cell = newVariable(-varObjects.size)
                regs[i] = ref(cell)
                pins[variable] = pinWord(ref(cell))
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
                    pins.entries
                        .filter { (variable, pin) ->
                            !variable.isAnonymous && heap[payload(pinnedWords[pin])].let { it != pinnedWords[pin] }
                        }.sortedBy { (_, pin) -> queryBound[pin] }
                        .associate { (variable, pin) -> variable to toTerm(pinnedWords[pin], names) },
                )
                found = runToSolution(first = false)
            }
        }

    private fun runToSolution(first: Boolean): Boolean =
        try {
            (first || backtrack(0)) && run(0)
        } catch (raised: Raised) {
            throw Halt(haltOf(raised))
        }

    companion object {
        const val NONE = -1
        const val FRAME = 4
        const val CP_SIZE = 17

        // The slots of a frame before the clause sets them
        private const val UNSET = (0 shl Words.TAG_BITS) or INT

        // The functors of the choice points that are not clause alternatives
        const val BARRIER = -2
        const val RESUME = -3
        const val PRIMITIVE = -4
        const val CATCH = -5
        const val SOURCED = -6
        const val NATIVE = -7

        // Where the clauses of a choice point come from: the libraries and the theory, or the theory alone
        private const val MERGED = 0
        private const val THEORY = 1

        // The outcomes of a call
        const val FAILED = 0
        const val CONTINUE = 1
        const val JUMPED = 2

        // The outcomes of unwinding a ball
        private const val UNCAUGHT = 0
        private const val RESUMED = 1
        private const val EXHAUSTED = 2

        // The heap cells above which a call collects
        const val GC_MIN = 1 shl 20

        // The calls between two looks at the clock
        private const val CHECK_EVERY = 1024

        /** The message of the halt of a resolution its caller interrupted */
        const val INTERRUPTED = "The resolution was interrupted by its caller"
    }
}
