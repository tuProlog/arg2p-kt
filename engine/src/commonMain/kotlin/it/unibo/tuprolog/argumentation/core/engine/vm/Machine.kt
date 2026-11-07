package it.unibo.tuprolog.argumentation.core.engine.vm

import it.unibo.tuprolog.argumentation.core.engine.ClauseObservers
import it.unibo.tuprolog.argumentation.core.engine.ClauseSource
import it.unibo.tuprolog.argumentation.core.engine.ExecutionEnvironment
import it.unibo.tuprolog.argumentation.core.engine.GoalWrapper
import it.unibo.tuprolog.argumentation.core.engine.MachineCall
import it.unibo.tuprolog.argumentation.core.engine.NativeCall
import it.unibo.tuprolog.argumentation.core.engine.NativePrimitive
import it.unibo.tuprolog.argumentation.core.engine.NativeResume
import it.unibo.tuprolog.argumentation.core.engine.PrologExecutionContext
import it.unibo.tuprolog.argumentation.core.engine.Record
import it.unibo.tuprolog.argumentation.core.engine.RecordKey
import it.unibo.tuprolog.argumentation.core.engine.RegisterGoalCall
import it.unibo.tuprolog.argumentation.core.engine.RegisterGoalPrimitive
import it.unibo.tuprolog.argumentation.core.engine.vm.Words.ATOM
import it.unibo.tuprolog.argumentation.core.engine.vm.Words.BIG
import it.unibo.tuprolog.argumentation.core.engine.vm.Words.FLT
import it.unibo.tuprolog.argumentation.core.engine.vm.Words.FUN
import it.unibo.tuprolog.argumentation.core.engine.vm.Words.INT
import it.unibo.tuprolog.argumentation.core.engine.vm.Words.LIST
import it.unibo.tuprolog.argumentation.core.engine.vm.Words.REF
import it.unibo.tuprolog.argumentation.core.engine.vm.Words.STR
import it.unibo.tuprolog.argumentation.core.runtime.context.ClauseStore
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
 * those of the theory of the user when it is visible: a module call ([NativeCall.inline]) switches them for its
 * goal and back when the goal is done (a frame of its own holds the scope to restore). A lookup
 * ([NativeCall.sourced]) resolves its goal with clauses read when it is made.
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
 * The primitives of the libraries ([facade]) are called with their goal as tuProlog terms and their answers unified
 * back; a primitive with further answers leaves a choice point of its own.
 */
internal class Machine(
    val program: Program,
    // The heap cells above which a call collects (the threshold then follows the cells kept)
    private val gcMinimum: Int = GC_MIN,
) : RegisterGoalCall,
    MachineCall {
    private val pooled = MachineArrays.take()
    var heap = pooled.heap
    var top = 0

    // What a cell holding an unbound variable is called when the term is read: 0 nothing, a name atom and one for a
    // variable the code made (fresh as read), minus an index and one for the variable object the cell was made for
    // (the same object as read again: a stored term read twice gives the same variables, as the engine does)
    private var varInfo = pooled.varInfo
    private val varObjects = ArrayList<Var>()

    // The cells of the variables of the query (the first ones), and when each was bound (the engine reported the
    // bindings of a solution in the order they were made)
    private var queryCells = 0
    private var queryBound = IntArray(16)
    private var bindings = 0

    // The cells below this address are trailed when bound (the heap top at the latest choice point)
    private var heapBarrier = 0
    private var trail = pooled.trail
    private var trailTop = 0

    // The local stack: environment frames (the previous frame, the continuation, the cut barrier, the number of
    // slots, then the slots)
    private var local = pooled.local

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

    private var cps = pooled.cps

    // By choice point, those of clauses: the clauses it goes on with, and the candidates among them if an index gave them
    private var cpTables = arrayOfNulls<Program.ClauseTable>(cps.size / CP_SIZE)
    private var cpCandidates = arrayOfNulls<IntArray>(cps.size / CP_SIZE)
    var cpTop = 0
        private set
    private var cpArgs = pooled.cpArgs
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
    private var marks = IntArray(0)
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

    // A lookup in a store with clauses left to try: the goals of their bodies wrapped; for `clause(Head, Body)`, its
    // Body in the register after the arguments of Head, unified with the stored body
    private class StoredCall(
        val candidates: ClauseStore.Candidates,
        val wrap: GoalWrapper?,
        val asClause: Boolean,
    )

    // The arity of the native primitive being called, and the variables its [term]s gave the cells (one call)
    private var nativeArity = 0
    private var nativeFunctor = 0
    private var nativeNames: HashMap<Int, Var>? = null

    // The cells the words of a native call gave the variables of its terms that were not cells already (fresh ones
    // of a clause copied, say): the same variable the same cell within the call
    private var nativeFresh: HashMap<Var, Int>? = null

    // How many of [nativeNames] are in [nativeFresh] already
    private var nativeMerged = 0

    // Those of the native calls a nested run is under, for a collection to find
    private val heldNames = ArrayList<HashMap<Int, Var>>()
    private val heldFresh = ArrayList<HashMap<Var, Int>>()

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
    private val smallValues = IntArray(32)
    private var exprWords = IntArray(32)

    // The instances of the runs of findall, those of the nested ones above: where each instance starts in the bag
    private var roots = IntArray(256)
    private var rootsTop = 0
    private var evalFailed = false

    private val wComma = program.functorWord(",", 2)
    private val wCatchExit = program.functorWord("\$catch_exit", 1)

    // The number of the last catch/3 run, telling its choice point apart (see catchExit)
    private var catches = 0
    private val wTrue = program.atomWord("true")
    private val fIfThen = program.functorWord("->", 2)
    private val fList = program.functor(".", 2)
    private val failGoal = program.atomWord("fail")

    // The code of the control constructs called as goals at run time, of the end of a run, of the return from an
    // inlined goal
    private val compiler = Compiler(program)

    // (once for each program: a machine for each solve would leave them behind in its code)
    private val controlBlocks =
        program.controlBlocks {
            intArrayOf(
                compiler.stopBlock(),
                compiler.conjunctionBlock(),
                compiler.disjunctionBlock(),
                compiler.ifThenElseBlock(),
                compiler.negationBlock(),
                compiler.restoreScopeBlock(),
            )
        }
    private val stopAddress = controlBlocks[0]
    private val conjunctionBlock = controlBlocks[1]
    private val disjunctionBlock = controlBlocks[2]
    private val ifThenElseBlock = controlBlocks[3]
    private val negationBlock = controlBlocks[4]
    private val restoreScopeBlock = controlBlocks[5]
    private val fArg = program.functor("arg", 3)
    private val fFunctor = program.functor("functor", 3)

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
        builtin("\$catch_exit", 1) { m -> m.catchExit() }
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
        val x1 = dereferenced(x0)
        val y1 = dereferenced(y0)
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
        return unifyCompounds(x1, y1)
    }

    // Two compound terms of the same tag, argument by argument
    private fun unifyCompounds(
        x1: Int,
        y1: Int,
    ): Boolean {
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
        val d = dereferenced(w)
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
        frame = e
    }

    @Suppress("NOTHING_TO_INLINE")
    private inline fun deallocate() {
        continuation = local[frame + 1]
        cutBarrier = local[frame + 2]
        frame = local[frame]
    }

    private var released = false

    /** The arrays back for another machine: this one is done, it runs no more */
    fun release() {
        if (released) return
        released = true
        pooled.heap = heap
        pooled.varInfo = varInfo
        pooled.trail = trail
        pooled.local = local
        pooled.cps = cps
        pooled.cpArgs = cpArgs
        MachineArrays.give(pooled)
    }

    // The choice point delayed: the clause entered has alternatives, and until it gets past its head and its guard nothing
    // else can see them, so they wait here, its arguments above the top of [cpArgs] (a head that fails, or a guard before a
    // cut, costs no choice point). -1 for none. What it does not hold is as the clause found it: the continuation and the
    // barrier, as a frame allocated meanwhile restores them
    private var delayedNext = -1
    private var delayedTable: Program.ClauseTable? = null
    private var delayedCandidates: IntArray? = null
    private var delayedFunctor = 0
    private var delayedArity = 0
    private var delayedKey = 0
    private var delayedTop = 0
    private var delayedTrail = 0
    private var delayedFrame = NONE

    // Whether the arguments are saved: a clause that only tests (see ClauseCode.guarded) leaves them where they are
    private var delayedSaved = false

    // Whether the arguments of the goal beyond the first are all unbound
    private var delayedFree = false

    private fun saveDelayedArguments(arity: Int) {
        if (cpArgsTop + arity > cpArgs.size) cpArgs = cpArgs.copyOf(maxOf(cpArgs.size * 2, cpArgsTop + arity))
        copyWords(regs, 0, cpArgs, cpArgsTop, arity)
        delayedSaved = true
    }

    // A few words at most, for the most part: a loop, not an array copy of a length the JIT of the JVM does not know
    private fun copyWords(
        from: IntArray,
        fromAt: Int,
        to: IntArray,
        toAt: Int,
        n: Int,
    ) {
        var i = 0
        while (i < n) {
            to[toAt + i] = from[fromAt + i]
            i++
        }
    }

    private fun delayChoicePoint(
        table: Program.ClauseTable,
        candidates: IntArray?,
        functor: Int,
        entered: Program.ClauseCode,
        next: Int,
        arity: Int,
        key: Int,
    ) {
        if (entered.guarded) delayedSaved = false else saveDelayedArguments(arity)
        if (!table.pinned) table.pinned = true
        delayedNext = next
        delayedTable = table
        delayedCandidates = candidates
        delayedFunctor = functor
        delayedArity = arity
        delayedKey = key
        delayedTop = top
        delayedTrail = trailTop
        delayedFrame = frame
        heapBarrier = top
    }

    // The choice point delayed made, if any: the clause goes beyond what it can undo alone
    @Suppress("NOTHING_TO_INLINE")
    private inline fun settle() {
        if (delayedNext >= 0) madeChoicePoint()
    }

    // What arms of [execute] do through calls, the reads staying out of it: a field two arms or more read on entry, C2 reads
    // at every dispatch, for all of them
    private fun settleHere() = settle()

    private fun proceeded(): Int {
        settle()
        return continuation
    }

    private fun cutNeck() = cutTo(cutBarrier)

    // The alternative at [address] of the clause being run, delayed as well: a condition of tests costs no choice point
    private fun delayAlternative(address: Int) {
        settle()
        delayedNext = address
        delayedSaved = true
        delayedTable = null
        delayedArity = 0
        delayedKey = 0
        delayedTop = top
        delayedTrail = trailTop
        delayedFrame = frame
        heapBarrier = top
    }

    private fun madeChoicePoint() {
        val table = delayedTable
        if (!delayedSaved) saveDelayedArguments(delayedArity)
        val base = cpTop * CP_SIZE
        if (base + CP_SIZE > cps.size) growChoicePoints()
        cpTables[cpTop] = table
        cpCandidates[cpTop] = delayedCandidates
        cps[base] = delayedTrail
        cps[base + 1] = delayedTop
        cps[base + 2] = maxOf(frameEnd(delayedFrame), localMark())
        cps[base + 3] = delayedFrame
        cps[base + 4] = continuation
        if (table == null) {
            cps[base + 5] = delayedNext
            cps[base + 6] = RESUME
            cps[base + 7] = 0
            cps[base + 8] = 0
        } else {
            cps[base + 5] = 0
            cps[base + 6] = delayedFunctor
            cps[base + 7] = delayedNext
            cps[base + 8] = cutBarrier
        }
        cps[base + 9] = cpArgsTop
        cps[base + 10] = calls.size
        cps[base + 11] = delayedArity
        cps[base + 12] = delayedKey
        cps[base + 13] = scope.id
        cps[base + 14] = if (theoryVisible) 1 else 0
        cps[base + 15] = pinnedWordsTop
        cps[base + 16] = if (delayedFree && table != null) MERGED_FREE else MERGED
        cpArgsTop += delayedArity
        cpTop++
        delayedNext = -1
    }

    private fun dropDelayed() {
        delayedNext = -1
        heapBarrier = if (cpTop > 0) cps[(cpTop - 1) * CP_SIZE + 1] else 0
    }

    // The next clause of the choice point delayed, the machine as the call left it
    private fun retryDelayed(): Boolean {
        undoTrail(delayedTrail)
        top = delayedTop
        frame = delayedFrame
        val table = delayedTable
        if (table == null) {
            pc = delayedNext
            dropDelayed()
            return true
        }
        val arity = delayedArity
        if (delayedSaved) copyWords(cpArgs, cpArgsTop, regs, 0, arity)
        return retryClauses(table, delayedCandidates, delayedNext, arity, cutBarrier, delayedKey, delayedFree, NONE)
    }

    private fun growChoicePoints() {
        cps = cps.copyOf(cps.size * 2)
        cpTables = cpTables.copyOf(cpTables.size * 2)
        cpCandidates = cpCandidates.copyOf(cpCandidates.size * 2)
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
        settle()
        val base = cpTop * CP_SIZE
        if (base + CP_SIZE > cps.size) growChoicePoints()
        cpTables[cpTop] = null
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
        copyWords(regs, 0, cpArgs, cpArgsTop, arity)
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
        if (scope.id != cps[base + 13]) scope = program.scope(cps[base + 13])
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
        if (delayedNext >= 0) dropDelayed()
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
        if (delayedNext >= 0 && retryDelayed()) return true
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
                CATCH_EXIT -> {
                    // back into the goal of the catch: active again
                    cps[nextClause * CP_SIZE + 7] = 0
                    popChoicePoint()
                }
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
                    val alternatives = calls[nextClause] as NativeAlternatives
                    val argsBase = cps[base + 9]
                    val arity = cps[base + 11]
                    copyWords(cpArgs, argsBase, regs, 0, arity)
                    popChoicePoint()
                    nativeArity = arity
                    nativeNames = alternatives.names?.let { names -> namesBelowTop(names) }
                    nativeFresh = null
                    nativeMerged = 0
                    val outcome =
                        try {
                            alternatives.resume.resume(this)
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
                    val call = calls[nextClause]
                    val argsBase = cps[base + 9]
                    val arity = cps[base + 11]
                    val from = cps[base + 12]
                    copyWords(cpArgs, argsBase, regs, 0, arity)
                    popChoicePoint()
                    val answer =
                        if (call is StoredCall) {
                            tryStored(call, from, arity, nextClause)
                        } else {
                            trySourced(call as SourcedCall, from, arity, nextClause)
                        }
                    if (answer != FAILED) return true
                }
                else -> {
                    val argsBase = cps[base + 9]
                    val arity = cps[base + 11]
                    copyWords(cpArgs, argsBase, regs, 0, arity)
                    val cp = cpTop - 1
                    if (cpTables[cp] == null) selectTheoryClauses(functor, arity, cp, base)
                    if (retryClauses(
                            cpTables[cp]!!,
                            cpCandidates[cp],
                            nextClause,
                            arity,
                            barrier,
                            cps[base + 12],
                            cps[base + 16] == MERGED_FREE,
                            base,
                        )
                    ) {
                        return true
                    }
                }
            }
        }
        return false
    }

    // [backtrack] as far as the choice points of clauses and conditions go: false for [backtrack] to go on
    private fun backtrackHere(cpBase: Int): Boolean {
        if (delayedNext >= 0 && retryDelayed()) return true
        while (cpTop > cpBase) {
            val base = (cpTop - 1) * CP_SIZE
            val functor = cps[base + 6]
            if (functor == RESUME) {
                restoreTop()
                popChoicePoint()
                return true
            }
            if (functor < 0) return false
            restoreTop()
            val arity = cps[base + 11]
            copyWords(cpArgs, cps[base + 9], regs, 0, arity)
            val cp = cpTop - 1
            if (cpTables[cp] == null) selectTheoryClauses(functor, arity, cp, base)
            if (retryClauses(
                    cpTables[cp]!!,
                    cpCandidates[cp],
                    cps[base + 7],
                    arity,
                    cps[base + 8],
                    cps[base + 12],
                    cps[base + 16] == MERGED_FREE,
                    base,
                )
            ) {
                return true
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

    // The key of the argument [k] of the goal, left dereferenced for the head to find it so
    private fun keyOfArgument(k: Int): Int {
        val d = dereferenced(regs[k])
        regs[k] = d
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

    // Enters the first clause of [predicate] the goal on the registers may match, leaving a choice point if another
    // may too (by the keys of every argument of the clauses, the engine leaving no choice point for a clause whose head
    // did not unify); false when none does
    private fun tryClauses(
        predicate: Program.Predicate,
        arity: Int,
        barrier: Int,
    ): Boolean {
        val table = predicate.table
        val n = table.size
        // (one clause: its head decides, with no key to look at)
        if (n == 1) return entered(table, 0, barrier)
        val key = if (arity > 0) keyOfArgument(0) else 0
        val firstKeys = table.firstKeys
        if (n == 2 && key != 0) {
            // (two clauses with two first keys, as [] and [_|_]: the key decides, with no alternative to look for)
            val firstKey = firstKeys[0]
            val secondKey = firstKeys[1]
            if (firstKey != 0 && secondKey != 0 && firstKey != secondKey) {
                return when (key) {
                    firstKey -> entered(table, 0, barrier)
                    secondKey -> entered(table, 1, barrier)
                    else -> false
                }
            }
        }
        return tryAmongClauses(predicate, table, key, arity, barrier)
    }

    // [tryClauses] when more than the first key may be needed
    private fun tryAmongClauses(
        predicate: Program.Predicate,
        table: Program.ClauseTable,
        key: Int,
        arity: Int,
        barrier: Int,
    ): Boolean {
        val n = table.size
        val firstKeys = table.firstKeys
        if (n >= Program.Predicate.INDEXED_FROM) {
            val candidates = selected(predicate, key, arity)
            if (candidates != null) return tryCandidates(table, candidates, predicate.functor, arity, barrier, key)
        }
        // The clauses in order, over the flat tables: the first candidate by its first argument, its head deciding the
        // rest (by the keys within it too, if the clauses have them: the lookahead would not tell them apart); the
        // lookahead for another by every argument the clause keys
        var i = 0
        if (table.innerDeep) {
            while (i < n && !matches(table.clauses[i]!!, key, arity)) i++
        } else {
            while (i < n && !matches(firstKeys[i], key)) i++
        }
        if (i == n) return false
        val clause = table.clauses[i]!!
        if (i + 1 < n && !commits(clause)) {
            // (a guarded clause either fails, its alternatives then looked at closely, or cuts them)
            val free = !clause.guarded && table.deep && !table.innerDeep && laterArgumentsFree(arity)
            val j = nextCandidate(table, i + 1, key, arity, free || clause.guarded)
            if (j < n) {
                delayChoicePoint(table, null, predicate.functor, clause, j, arity, key)
                delayedFree = free
            }
        }
        return entered(table, i, barrier)
    }

    // Whether the clause entered goes past its head for sure, its neck cut then dropping any alternative
    private fun commits(clause: Program.ClauseCode): Boolean {
        val mask = clause.commitMask
        if (mask < 0) return false
        var bits = mask
        var k = 0
        while (bits != 0) {
            if (bits and 1 != 0 && tag(deref(regs[k])) != REF) return false
            bits = bits ushr 1
            k++
        }
        return true
    }

    // Whether no argument of the goal beyond the first is bound: no key of a clause there can exclude it
    private fun laterArgumentsFree(arity: Int): Boolean {
        var k = 1
        while (k < arity) {
            if (tag(deref(regs[k])) != REF) return false
            k++
        }
        return true
    }

    private fun entered(
        table: Program.ClauseTable,
        clause: Int,
        barrier: Int,
    ): Boolean {
        cutBarrier = barrier
        if (ClauseObservers.active) ClauseObservers.resolved(table.clauses[clause]!!.source)
        pc = table.starts[clause]
        return true
    }

    // The clauses of a large [predicate] the goal on the registers may match, if an index says
    private fun selected(
        predicate: Program.Predicate,
        key: Int,
        arity: Int,
    ): IntArray? = if (key != 0) narrowed(predicate, predicate.candidates(key), arity) else deepCandidates(predicate, arity)

    // The clauses of a large [predicate] a goal with no first argument may match, if the index of the next bound
    // argument that has one says
    private fun deepCandidates(
        predicate: Program.Predicate,
        arity: Int,
    ): IntArray? {
        if (!predicate.deep) return null
        val end = minOf(arity - 1, Program.Predicate.MAX_DEEP_POSITION)
        var position = 1
        while (position <= end) {
            val k = keyOfArgument(position)
            if (k != 0) predicate.candidatesAt(position, k)?.let { return it }
            position++
        }
        return null
    }

    // The clauses [byFirst] the first argument leaves or, if they are many, those of the shortest list another argument has
    private fun narrowed(
        predicate: Program.Predicate,
        byFirst: IntArray,
        arity: Int,
    ): IntArray {
        if (byFirst.size < Program.Predicate.INDEXED_FROM || !predicate.deep) return byFirst
        var best = byFirst
        val end = minOf(arity - 1, Program.Predicate.MAX_DEEP_POSITION)
        var position = 1
        while (position <= end) {
            val k = keyOfArgument(position)
            if (k != 0) {
                val candidates = predicate.candidatesAt(position, k)
                if (candidates != null && candidates.size < best.size) best = candidates
            }
            position++
        }
        return best
    }

    // [tryClauses] over the clauses an index gave
    private fun tryCandidates(
        table: Program.ClauseTable,
        candidates: IntArray,
        functor: Int,
        arity: Int,
        barrier: Int,
        key: Int,
    ): Boolean {
        val firstKeys = table.firstKeys
        val size = candidates.size
        // (one clause: its head decides)
        if (size == 1) return entered(table, candidates[0], barrier)
        var i = 0
        if (table.innerDeep) {
            while (i < size && !matches(table.clauses[candidates[i]]!!, key, arity)) i++
        } else {
            while (i < size && !matches(firstKeys[candidates[i]], key)) i++
        }
        if (i == size) return false
        val clause = table.clauses[candidates[i]]!!
        if (i + 1 < size && !commits(clause)) {
            val q = nextPosition(table, candidates, i + 1, key, arity, clause.guarded)
            if (q < size) {
                delayChoicePoint(table, candidates, functor, clause, q, arity, key)
                delayedFree = false
            }
        }
        return entered(table, candidates[i], barrier)
    }

    // The alternatives a builtin left of the clauses of the theory, selected now that they are tried
    private fun selectTheoryClauses(
        functor: Int,
        arity: Int,
        cp: Int,
        base: Int,
    ) {
        val predicate = program.theory.predicate(functor)!!
        val table = predicate.table
        if (!table.pinned) table.pinned = true
        val key = if (arity > 0) keyOfArgument(0) else 0
        cpTables[cp] = table
        cpCandidates[cp] = if (table.size >= Program.Predicate.INDEXED_FROM) selected(predicate, key, arity) else null
        cps[base + 12] = key
    }

    // The next clause of the choice point on top (at [base]; [NONE]: of the one delayed) from the position [from] among its
    // clauses or its candidates, the registers holding its arguments: the choice point stays for the one after it, if any,
    // and goes otherwise; false when none is left
    private fun retryClauses(
        table: Program.ClauseTable,
        candidates: IntArray?,
        from: Int,
        arity: Int,
        barrier: Int,
        key: Int,
        free: Boolean,
        base: Int,
    ): Boolean {
        val firstKeys = table.firstKeys
        val clause: Int
        val next: Int
        val more: Boolean
        if (candidates != null) {
            val size = candidates.size
            var p = from
            while (p < size && !matches(firstKeys[candidates[p]], key)) p++
            if (p >= size) {
                dropAlternatives(base)
                return false
            }
            clause = candidates[p]
            next = nextPosition(table, candidates, p + 1, key, arity, false)
            more = next < size
        } else {
            val n = table.size
            var first = from
            while (first < n && !matches(firstKeys[first], key)) first++
            if (first >= n) {
                dropAlternatives(base)
                return false
            }
            clause = first
            next = nextCandidate(table, first + 1, key, arity, free)
            more = next < n
        }
        when {
            !more -> dropAlternatives(base)
            base < 0 -> {
                delayedNext = next
                if (!delayedSaved && !table.clauses[clause]!!.guarded) saveDelayedArguments(arity)
            }
            else -> {
                truncateCalls(cps[base + 10])
                cps[base + 7] = next
            }
        }
        return entered(table, clause, barrier)
    }

    // No clause is left of the choice point at [base], or of the one delayed
    private fun dropAlternatives(base: Int) {
        if (base < 0) dropDelayed() else popChoicePoint()
    }

    // The first clause from [from] on the keys of all its arguments do not exclude
    private fun nextCandidate(
        table: Program.ClauseTable,
        from: Int,
        key: Int,
        arity: Int,
        free: Boolean,
    ): Int {
        val n = table.size
        val firstKeys = table.firstKeys
        var j = from
        if (table.deep && !free) {
            while (j < n && !(matches(firstKeys[j], key) && matches(table.clauses[j]!!, key, arity))) j++
        } else {
            while (j < n && !matches(firstKeys[j], key)) j++
        }
        return j
    }

    // [nextCandidate] among the clauses an index gave: its position among them
    private fun nextPosition(
        table: Program.ClauseTable,
        candidates: IntArray,
        from: Int,
        key: Int,
        arity: Int,
        free: Boolean,
    ): Int {
        val size = candidates.size
        val firstKeys = table.firstKeys
        var q = from
        if (table.deep && !free) {
            while (q < size && !(matches(firstKeys[candidates[q]], key) && matches(table.clauses[candidates[q]]!!, key, arity))) q++
        } else {
            while (q < size && !matches(firstKeys[candidates[q]], key)) q++
        }
        return q
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
            if (ck != 0 && !matches(ck, keyOfArgument(k))) return false
        }
        val inner = clause.innerKeys
        return inner.isEmpty() || innerMatches(inner)
    }

    // Whether the arguments of the first argument of the goal may match the keys of those of a clause
    private fun innerMatches(inner: IntArray): Boolean {
        val d = deref(regs[0])
        val base =
            when (tag(d)) {
                STR -> payload(d) + 1
                LIST -> payload(d)
                else -> return true
            }
        for (i in inner.indices) {
            val ck = inner[i]
            if (ck != 0 && !matches(ck, keyOfHeap(heap[base + i]))) return false
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
        return if (tryClauses(predicate, arity, cpTop)) JUMPED else FAILED
    }

    // A call of an instruction: the address to go on at after it, -1 if it failed; [next] the address to come back to,
    // negative for a goal in place (the one entry, for the JIT of the JVM to compile what is under it once)
    private fun goal(
        functor: Int,
        arity: Int,
        next: Int,
    ): Int {
        settle()
        if (next >= 0) {
            continuation = next
            pc = next
        }
        cutBarrier = cpTop
        return when (call(functor, arity)) {
            FAILED -> -1
            JUMPED -> pc
            else -> if (next >= 0) next else continuation
        }
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
        val native = scope.native(functor)
        if (native != null) {
            val answer = native.run(this)
            if (answer != StdlibNatives.NOT_NATIVE) return answer
        }
        return callPrimitive(primitive, functor, arity)
    }

    /** Calls the goal [w], a term on the heap, its arguments loaded in the registers */
    fun callTerm(w: Int): Int {
        val g = deref(w)
        val functor: Int
        val arity: Int
        when (tag(g)) {
            ATOM -> {
                functor = program.atomFunctor(payload(g))
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
                        if (!unify(regs[code[at + 1]], regs[code[at + 2]])) break@dispatch
                        ip = at + 3
                    }
                    Op.GET_Y_VAL -> {
                        if (!unify(local[frame + FRAME + code[at + 1]], regs[code[at + 2]])) break@dispatch
                        ip = at + 3
                    }
                    Op.GET_CONST -> {
                        if (!unifyConstant(code[at + 1], regs[code[at + 2]])) break@dispatch
                        ip = at + 3
                    }
                    Op.GET_LIST_VV -> {
                        val d = dereferenced(regs[code[at + 1]])
                        when (tag(d)) {
                            REF -> writeListOfVariables(code, at, d)
                            LIST -> {
                                val a = payload(d)
                                regs[code[at + 2]] = heap[a]
                                regs[code[at + 4]] = heap[a + 1]
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
                        if (!unify(regs[code[at + 1]], regs[code[at + 2]])) break@dispatch
                        ip = proceeded()
                    }
                    Op.GET_CONST_PROCEED -> {
                        if (!unifyConstant(code[at + 1], regs[code[at + 2]])) break@dispatch
                        ip = proceeded()
                    }
                    Op.GET_CONST2 -> {
                        if (!unifyConstant(code[at + 1], regs[code[at + 2]]) ||
                            !unifyConstant(code[at + 3], regs[code[at + 4]])
                        ) {
                            break@dispatch
                        }
                        ip = at + 5
                    }
                    Op.GET_CONST2_PROCEED -> {
                        if (!unifyConstant(code[at + 1], regs[code[at + 2]]) ||
                            !unifyConstant(code[at + 3], regs[code[at + 4]])
                        ) {
                            break@dispatch
                        }
                        ip = proceeded()
                    }
                    Op.GET_LIST2_GET_X_VAL -> {
                        if (!getList2(code, at) || !unify(regs[code[at + 8]], regs[code[at + 9]])) break@dispatch
                        ip = at + 10
                    }
                    Op.GET_LIST2_GET_X_VAL_PROCEED -> {
                        if (!getList2(code, at) || !unify(regs[code[at + 8]], regs[code[at + 9]])) break@dispatch
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
                            FAILED -> break@dispatch
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
                            FAILED -> break@dispatch
                            CONTINUE -> ip = continuation
                            else -> ip = pc
                        }
                        code = program.code
                    }
                    Op.PUT_STR -> {
                        val h = alloc(1)
                        heap[h] = code[at + 1]
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
                        val h = newVariable(code[at + 3])
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
                        if (!unifyConstant(code[at + 1], regs[code[at + 2]])) break@dispatch
                        cutNeck()
                        ip = continuation
                    }
                    Op.CUT_CALL -> {
                        cutTo(local[frame + 2])
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
                        if ((code[at + 3] ushr tag(dereferenced(regs[code[at + 4]]))) and 1 == 0) break@dispatch
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
                        val index = dereferenced(operand(code[at + 1]))
                        if (tag(index) == INT && payload(index) >= 0) {
                            val answer = inlineArg(payload(index), operand(code[at + 2]), Op.T_NEW_Y, code[at + 3])
                            if (answer == FAILED) break@dispatch
                            ip = if (answer == CONTINUE) at + 5 + code[at + 4] else at + 5
                        } else {
                            ip = at + 5
                        }
                    }
                    Op.GET_STR_VV -> {
                        val d = dereferenced(regs[code[at + 2]])
                        when (tag(d)) {
                            STR -> {
                                val a = payload(d)
                                if (heap[a] != code[at + 1]) break@dispatch
                                regs[code[at + 4]] = heap[a + 1]
                                regs[code[at + 7]] = heap[a + 2]
                            }
                            REF -> writeStructureOfVariables(code, at, d)
                            else -> break@dispatch
                        }
                        ip = at + 9
                    }
                    Op.GET_LIST_VX_EXECUTE -> {
                        val d = dereferenced(regs[code[at + 1]])
                        when (tag(d)) {
                            REF -> {
                                val h = top
                                if (h + 2 > heap.size) grow(h + 2)
                                top = h + 2
                                bind(payload(d), word(LIST, h))
                                heap[h] = regs[code[at + 3]]
                                heap[h + 1] = ref(h + 1)
                                varInfo[h + 1] = code[at + 7]
                                regs[code[at + 6]] = ref(h + 1)
                            }
                            LIST -> {
                                val a = payload(d)
                                if (!unify(regs[code[at + 3]], heap[a])) break@dispatch
                                regs[code[at + 6]] = heap[a + 1]
                            }
                            else -> break@dispatch
                        }
                        ip = goal(code[at + 8], code[at + 9], -1)
                        if (ip < 0) break@dispatch
                        code = program.code
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
                    Op.PROCEED -> {
                        ip = proceeded()
                    }
                    Op.CUT_NECK -> {
                        cutNeck()
                        ip = at + 1
                    }
                    Op.CUT -> {
                        cutTo(local[frame + 2])
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
                        cutBarrier = if (code[at + 2] == Op.TRANSPARENT) local[frame + 2] else cpTop
                        pc = ip
                        when (callTerm(goal)) {
                            FAILED -> break@dispatch
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
                            FAILED -> break@dispatch
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
                        val a = dereferenced(operand(code[at + 2]))
                        val b = dereferenced(operand(code[at + 3]))
                        if (tag(a) == INT && tag(b) == INT) {
                            if (!holds(code[at + 1], payload(a), payload(b))) break@dispatch
                            ip = at + 5 + code[at + 4]
                        } else {
                            ip = at + 5
                        }
                    }
                    Op.COMPARE_VC -> {
                        val a = dereferenced(operand(code[at + 2]))
                        if (tag(a) == INT) {
                            if (!holds(code[at + 1], payload(a), code[at + 3])) break@dispatch
                            ip = at + 5 + code[at + 4]
                        } else {
                            ip = at + 5
                        }
                    }
                    Op.IS_VC -> {
                        val a = dereferenced(operand(code[at + 1]))
                        if (tag(a) == INT) {
                            val w = program.sumWord(payload(a), code[at + 2])
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
                        val w = arithmeticWord(code, at + 4, n)
                        if (w != 0) {
                            val operand = code[at + 2]
                            when (code[at + 1]) {
                                Op.T_X -> if (!unify(regs[operand], w)) break@dispatch
                                Op.T_Y -> if (!unify(local[frame + FRAME + operand], w)) break@dispatch
                                Op.T_NEW_X -> regs[operand] = w
                                Op.T_NEW_Y -> local[frame + FRAME + operand] = w
                                else -> if (operand != w) break@dispatch
                            }
                        } else {
                            // The builtin: the target and the expression as words in the registers, which are kept
                            // (the expression first: a target new to the clause may take the register of an operand)
                            val expression = build(code, at + 4, n)
                            ip = builtinBehind(fIs, targetWord(code[at + 1], code[at + 2]), expression, ip)
                            if (ip < 0) break@dispatch
                            code = program.code
                        }
                    }
                    Op.ARITH_CMP -> {
                        val nA = code[at + 2]
                        val nB = code[at + 3 + nA]
                        ip = at + 4 + nA + nB
                        val answer = comparison(code, at, nA, nB)
                        if (answer == 0) break@dispatch
                        if (answer < 0) {
                            settleHere()
                            val left = build(code, at + 3, nA)
                            ip = builtinBehind(comparisonFunctors[code[at + 1]], left, build(code, at + 4 + nA, nB), ip)
                            if (ip < 0) break@dispatch
                            code = program.code
                        }
                    }
                    Op.TYPE_X -> {
                        if ((code[at + 1] ushr tag(dereferenced(regs[code[at + 2]]))) and 1 == 0) break@dispatch
                        ip = at + 3
                    }
                    Op.TYPE_Y -> {
                        if ((code[at + 1] ushr tag(deref(local[frame + FRAME + code[at + 2]]))) and 1 == 0) break@dispatch
                        ip = at + 3
                    }
                    Op.IDENTICAL -> {
                        if (StdlibNatives.identical(this, operand(code[at + 2]), operand(code[at + 3])) ==
                            (code[at + 1] != 0)
                        ) {
                            break@dispatch
                        }
                        ip = at + 4
                    }
                    Op.IDENTICAL_C -> {
                        if ((deref(operand(code[at + 2])) == code[at + 3]) == (code[at + 1] != 0)) break@dispatch
                        ip = at + 4
                    }
                    Op.ARG -> {
                        ip = at + 5
                        when (inlineArg(code[at + 1], operand(code[at + 2]), code[at + 3], code[at + 4])) {
                            FAILED -> break@dispatch
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
                        val index = deref(regs[0])
                        val answer =
                            if (tag(index) == INT &&
                                payload(index) >= 0
                            ) {
                                inlineArg(payload(index), regs[1], Op.T_X, 2)
                            } else {
                                NOT_INLINE
                            }
                        when (answer) {
                            FAILED -> break@dispatch
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
                            FAILED -> break@dispatch
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
                        local[frame + FRAME + code[at + 1]] = ref(newVariable(code[at + 2]))
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

    // ---- inline arithmetic

    // What 64 bits cannot hold is left to the library, which computes it whatever its size
    private fun additionOverflows(
        a: Long,
        b: Long,
        r: Long,
    ): Boolean = ((a xor r) and (b xor r)) < 0

    private fun subtractionOverflows(
        a: Long,
        b: Long,
        r: Long,
    ): Boolean = ((a xor b) and (a xor r)) < 0

    private fun productOverflows(
        a: Long,
        b: Long,
        r: Long,
    ): Boolean {
        // (two integers of 32 bits never do)
        if (((a + 0x80000000L) or (b + 0x80000000L)) ushr 32 == 0L) return false
        return (a == -1L && b == Long.MIN_VALUE) || (a != 0L && r / a != b)
    }

    private fun holds(
        kind: Int,
        a: Int,
        b: Int,
    ): Boolean =
        when (kind) {
            Op.C_LT -> a < b
            Op.C_GT -> a > b
            Op.C_LE -> a <= b
            Op.C_GE -> a >= b
            Op.C_EQ -> a == b
            else -> a != b
        }

    // The word of the value of the expression code at [from] ([n] words): 0 for what is left to the builtin
    private fun arithmeticWord(
        code: IntArray,
        from: Int,
        n: Int,
    ): Int {
        val small = evaluateSmall(code, from, n)
        if (small != NOT_SMALL) return program.smallWord(small)
        val value = evaluate(code, from, n)
        return if (evalFailed) 0 else program.intWord(value)
    }

    // Whether the comparison ARITH_CMP at [at] holds: 1 or 0, -1 for what is left to the builtin
    private fun comparison(
        code: IntArray,
        at: Int,
        nA: Int,
        nB: Int,
    ): Int {
        val smallA = evaluateSmall(code, at + 3, nA)
        if (smallA != NOT_SMALL) {
            val smallB = evaluateSmall(code, at + 4 + nA, nB)
            if (smallB != NOT_SMALL) return if (holds(code[at + 1], smallA, smallB)) 1 else 0
        }
        val a = evaluate(code, at + 3, nA)
        val b = if (evalFailed) 0L else evaluate(code, at + 4 + nA, nB)
        if (evalFailed) return -1
        val holds =
            when (code[at + 1]) {
                Op.C_LT -> a < b
                Op.C_GT -> a > b
                Op.C_LE -> a <= b
                Op.C_GE -> a >= b
                Op.C_EQ -> a == b
                else -> a != b
            }
        return if (holds) 1 else 0
    }

    // [evaluate] within 32 bits, with no Long (a BigInt on JS): [NOT_SMALL] for an operand that is no small integer, for a
    // step out of them, or for an error: [evaluate] then says what it is
    private fun evaluateSmall(
        code: IntArray,
        from: Int,
        n: Int,
    ): Int {
        val stack = smallValues
        var sp = 0
        var i = from
        val end = from + n
        while (i < end) {
            val kind = code[i]
            val operand = code[i + 1]
            i += 2
            when (kind) {
                Op.E_X, Op.E_Y -> {
                    val w = dereferenced(if (kind == Op.E_X) regs[operand] else local[frame + FRAME + operand])
                    if (tag(w) != INT || sp == stack.size) return NOT_SMALL
                    stack[sp++] = payload(w)
                }
                Op.E_CONST -> {
                    if (sp == stack.size) return NOT_SMALL
                    stack[sp++] = payload(operand)
                }
                Op.E_ADD -> {
                    sp--
                    val x = stack[sp - 1]
                    val y = stack[sp]
                    val r = x + y
                    if ((x xor r) and (y xor r) < 0) return NOT_SMALL
                    stack[sp - 1] = r
                }
                Op.E_SUB -> {
                    sp--
                    val x = stack[sp - 1]
                    val y = stack[sp]
                    val r = x - y
                    if ((x xor y) and (x xor r) < 0) return NOT_SMALL
                    stack[sp - 1] = r
                }
                Op.E_MUL -> {
                    sp--
                    val x = stack[sp - 1]
                    val y = stack[sp]
                    if (x < -SMALL_FACTOR || x > SMALL_FACTOR || y < -SMALL_FACTOR || y > SMALL_FACTOR) return NOT_SMALL
                    stack[sp - 1] = x * y
                }
                Op.E_IDIV -> {
                    sp--
                    val x = stack[sp - 1]
                    val y = stack[sp]
                    if (y == 0 || x == Int.MIN_VALUE) return NOT_SMALL
                    stack[sp - 1] = x / y
                }
                Op.E_MOD -> {
                    sp--
                    val y = stack[sp]
                    if (y == 0) return NOT_SMALL
                    stack[sp - 1] = stack[sp - 1].mod(y)
                }
                else -> {
                    if (stack[sp - 1] == Int.MIN_VALUE) return NOT_SMALL
                    stack[sp - 1] = -stack[sp - 1]
                }
            }
        }
        return stack[0]
    }

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
                            BIG -> {
                                if (payload(w) < 0) {
                                    evalFailed = true
                                    return 0
                                }
                                program.bigs[payload(w)]
                            }
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
                    val a = values[sp - 1]
                    val b = values[sp]
                    val r = a + b
                    if (additionOverflows(a, b, r)) {
                        evalFailed = true
                        return 0
                    }
                    values[sp - 1] = r
                }
                Op.E_SUB -> {
                    sp--
                    val a = values[sp - 1]
                    val b = values[sp]
                    val r = a - b
                    if (subtractionOverflows(a, b, r)) {
                        evalFailed = true
                        return 0
                    }
                    values[sp - 1] = r
                }
                Op.E_MUL -> {
                    sp--
                    val a = values[sp - 1]
                    val b = values[sp]
                    val r = a * b
                    if (productOverflows(a, b, r)) {
                        evalFailed = true
                        return 0
                    }
                    values[sp - 1] = r
                }
                Op.E_IDIV -> {
                    sp--
                    val divisor = values[sp]
                    if (divisor == 0L || (divisor == -1L && values[sp - 1] == Long.MIN_VALUE)) {
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
                else -> {
                    if (values[sp - 1] == Long.MIN_VALUE) {
                        evalFailed = true
                        return 0
                    }
                    values[sp - 1] = -values[sp - 1]
                }
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
    // The builtin an inline instruction leaves a case to, going on at [next]: the continuation and the barrier of the
    // clause stay, as the clause may have no frame to find them in
    private fun callBehindInstruction(
        functor: Int,
        arity: Int,
        next: Int,
    ): Int {
        settle()
        val savedContinuation = continuation
        val savedBarrier = cutBarrier
        continuation = next
        cutBarrier = cpTop
        pc = next
        // (no collection here: the temporaries of the clause are live, and they are no roots)
        val threshold = gcThreshold
        gcThreshold = Int.MAX_VALUE
        val outcome =
            try {
                call(functor, arity)
            } finally {
                gcThreshold = threshold
            }
        if (outcome == CONTINUE) {
            continuation = savedContinuation
            cutBarrier = savedBarrier
        }
        return outcome
    }

    // A builtin in place of what an instruction did not do inline, its arguments in the first registers, which are kept:
    // the address to go on at, -1 if it failed
    private fun builtinBehind(
        functor: Int,
        first: Int,
        second: Int,
        next: Int,
    ): Int {
        val a0 = regs[0]
        val a1 = regs[1]
        regs[0] = first
        regs[1] = second
        return when (callBehindInstruction(functor, 2, next)) {
            FAILED -> -1
            JUMPED -> pc
            else -> {
                regs[0] = a0
                regs[1] = a1
                next
            }
        }
    }

    private fun builtinBehind3(
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
        return when (callBehindInstruction(functor, 3, next)) {
            FAILED -> -1
            JUMPED -> pc
            else -> {
                regs[0] = a0
                regs[1] = a1
                regs[2] = a2
                next
            }
        }
    }

    // GET_STR_VV at [at] against the variable [d]
    private fun writeStructureOfVariables(
        code: IntArray,
        at: Int,
        d: Int,
    ) {
        val h = top
        if (h + 3 > heap.size) grow(h + 3)
        top = h + 3
        bind(payload(d), word(STR, h))
        heap[h] = code[at + 1]
        heap[h + 1] = ref(h + 1)
        varInfo[h + 1] = code[at + 5]
        heap[h + 2] = ref(h + 2)
        varInfo[h + 2] = code[at + 8]
        regs[code[at + 4]] = ref(h + 1)
        regs[code[at + 7]] = ref(h + 2)
    }

    // GET_LIST_VV at [at] against the variable [d]
    private fun writeListOfVariables(
        code: IntArray,
        at: Int,
        d: Int,
    ) {
        val h = alloc(2)
        heap[h] = ref(h)
        varInfo[h] = code[at + 3]
        heap[h + 1] = ref(h + 1)
        varInfo[h + 1] = code[at + 5]
        regs[code[at + 2]] = ref(h)
        regs[code[at + 4]] = ref(h + 1)
        bind(payload(d), word(LIST, h))
    }

    // The word of an operand of an inline builtin: a register, or a slot as -1 - slot
    private fun operand(o: Int): Int = if (o >= 0) regs[o] else local[frame + FRAME - 1 - o]

    private fun targetWord(
        kind: Int,
        operand: Int,
    ): Int =
        when (kind) {
            Op.T_X -> regs[operand]
            Op.T_Y -> local[frame + FRAME + operand]
            Op.T_NEW_X -> ref(newVariable()).also { regs[operand] = it }
            Op.T_NEW_Y -> ref(newVariable()).also { local[frame + FRAME + operand] = it }
            Op.T_VOID -> ref(newVariable())
            else -> operand
        }

    // The word [w] as the value of a target
    private fun toTarget(
        kind: Int,
        operand: Int,
        w: Int,
    ): Boolean =
        when (kind) {
            Op.T_X -> unify(regs[operand], w)
            Op.T_Y -> unify(local[frame + FRAME + operand], w)
            Op.T_NEW_X -> {
                regs[operand] = w
                true
            }
            Op.T_NEW_Y -> {
                local[frame + FRAME + operand] = w
                true
            }
            Op.T_VOID -> true
            else -> unify(operand, w)
        }

    // arg/3 with the index [i]: [CONTINUE] or [FAILED], [NOT_INLINE] for what is left to the builtin (what is not compound)
    private fun inlineArg(
        i: Int,
        term0: Int,
        kind: Int,
        operand: Int,
    ): Int {
        val term = dereferenced(term0)
        val w =
            when (tag(term)) {
                STR -> {
                    val a = payload(term)
                    if (i < 1 || i > program.arity(heap[a])) return FAILED
                    heap[a + i]
                }
                LIST -> {
                    if (i < 1 || i > 2) return FAILED
                    heap[payload(term) + i - 1]
                }
                else -> return NOT_INLINE
            }
        return if (toTarget(kind, operand, w)) CONTINUE else FAILED
    }

    // functor/3 of a term that is bound: [CONTINUE] or [FAILED], [NOT_INLINE] for what is left to the builtin (a term to build, an arity that is not one)
    private fun inlineFunctor(
        term0: Int,
        nameKind: Int,
        nameOperand: Int,
        arityKind: Int,
        arityOperand: Int,
    ): Int {
        val term = dereferenced(term0)
        val t = tag(term)
        if (t == REF) return inlineFunctorBuilding(term, nameKind, nameOperand, arityKind, arityOperand)
        if (arityKind == Op.T_X || arityKind == Op.T_Y || arityKind == Op.T_CONST) {
            val arity = dereferenced(targetWord(arityKind, arityOperand))
            if (tag(arity) != REF && (tag(arity) != INT || payload(arity) < 0)) return NOT_INLINE
        }
        val name: Int
        val n: Int
        when (t) {
            STR -> {
                val f = heap[payload(term)]
                name = word(ATOM, program.functorNames[payload(f)])
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
            FAILED
        }
    }

    // functor/3 with the variable [term]: the compound of a name and of a positive arity built, the rest left to the builtin
    private fun inlineFunctorBuilding(
        term: Int,
        nameKind: Int,
        nameOperand: Int,
        arityKind: Int,
        arityOperand: Int,
    ): Int {
        if (nameKind == Op.T_NEW_X || nameKind == Op.T_NEW_Y || nameKind == Op.T_VOID) return NOT_INLINE
        if (arityKind == Op.T_NEW_X || arityKind == Op.T_NEW_Y || arityKind == Op.T_VOID) return NOT_INLINE
        val name = dereferenced(targetWord(nameKind, nameOperand))
        val arity = dereferenced(targetWord(arityKind, arityOperand))
        if (tag(name) != ATOM || tag(arity) != INT || payload(arity) <= 0) return NOT_INLINE
        val n = payload(arity)
        val built: Int
        if (name == program.dotWord) {
            if (n != 2) return NOT_INLINE
            built = word(LIST, top)
            pushVoids(2)
        } else {
            val h = alloc(1)
            heap[h] = word(FUN, program.functor(payload(name), n))
            pushVoids(n)
            built = word(STR, h)
        }
        bind(payload(term), built)
        return CONTINUE
    }

    // [deref] in place: the JIT of the JVM judges most of the sites calling it cold, and leaves them calls
    @Suppress("NOTHING_TO_INLINE")
    private inline fun dereferenced(w0: Int): Int {
        var w = w0
        while (tag(w) == REF) {
            val c = heap[payload(w)]
            if (c == w) return w
            w = c
        }
        return w
    }

    // The instruction GET_LIST2 at [at] of [code]
    private fun getList2(
        code: IntArray,
        at: Int,
    ): Boolean {
        val d = dereferenced(regs[code[at + 1]])
        when (tag(d)) {
            LIST -> {
                val a = payload(d)
                return readCell(code[at + 2], code[at + 3], a) && readCell(code[at + 5], code[at + 6], a + 1)
            }
            REF -> {
                val h = top
                if (h + 2 > heap.size) grow(h + 2)
                top = h + 2
                bind(payload(d), word(LIST, h))
                writeCell(code[at + 2], code[at + 3], code[at + 4], h)
                writeCell(code[at + 5], code[at + 6], code[at + 7], h + 1)
                return true
            }
            else -> return false
        }
    }

    // The instruction GET_STR2 at [at] of [code]
    private fun getStructure2(
        code: IntArray,
        at: Int,
    ): Boolean {
        val f = code[at + 1]
        val d = dereferenced(regs[code[at + 2]])
        when (tag(d)) {
            STR -> {
                val a = payload(d)
                if (heap[a] != f) return false
                return readCell(code[at + 3], code[at + 4], a + 1) && readCell(code[at + 6], code[at + 7], a + 2)
            }
            REF -> {
                val h = top
                if (h + 3 > heap.size) grow(h + 3)
                top = h + 3
                heap[h] = f
                bind(payload(d), word(STR, h))
                writeCell(code[at + 3], code[at + 4], code[at + 5], h + 1)
                writeCell(code[at + 6], code[at + 7], code[at + 8], h + 2)
                return true
            }
            else -> return false
        }
    }

    // The instruction GET_STRN at [at] of [code]
    private fun getStructureN(
        code: IntArray,
        at: Int,
    ): Boolean {
        val f = code[at + 1]
        val n = code[at + 3]
        val d = dereferenced(regs[code[at + 2]])
        var sub = at + 4
        when (tag(d)) {
            STR -> {
                val a = payload(d)
                if (heap[a] != f) return false
                for (i in 1..n) {
                    if (!readCell(code[sub], code[sub + 1], a + i)) return false
                    sub += 3
                }
                return true
            }
            REF -> {
                val h = top
                if (h + n + 1 > heap.size) grow(h + n + 1)
                top = h + n + 1
                heap[h] = f
                bind(payload(d), word(STR, h))
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
    private fun putStructure2(
        code: IntArray,
        at: Int,
    ) {
        val h = top
        if (h + 3 > heap.size) grow(h + 3)
        top = h + 3
        heap[h] = code[at + 1]
        regs[code[at + 2]] = word(STR, h)
        writeCell(code[at + 3], code[at + 4], code[at + 5], h + 1)
        writeCell(code[at + 6], code[at + 7], code[at + 8], h + 2)
    }

    // The instruction PUT_LIST2 at [at] of [code]
    private fun putList2(
        code: IntArray,
        at: Int,
    ) {
        val h = top
        if (h + 2 > heap.size) grow(h + 2)
        top = h + 2
        regs[code[at + 1]] = word(LIST, h)
        writeCell(code[at + 2], code[at + 3], code[at + 4], h)
        writeCell(code[at + 5], code[at + 6], code[at + 7], h + 1)
    }

    // A sub-instruction (see Op) on the argument of a structure at [address]
    @Suppress("NOTHING_TO_INLINE")
    private inline fun readCell(
        kind: Int,
        a: Int,
        address: Int,
    ): Boolean {
        when (kind) {
            Op.U_X_VAR -> regs[a] = heap[address]
            Op.U_Y_VAR -> local[frame + FRAME + a] = heap[address]
            Op.U_X_VAL -> return unify(regs[a], heap[address])
            Op.U_Y_VAL -> return unify(local[frame + FRAME + a], heap[address])
            Op.U_CONST -> {
                val w = dereferenced(heap[address])
                if (tag(w) == REF) {
                    bind(payload(w), a)
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
    private inline fun writeCell(
        kind: Int,
        a: Int,
        b: Int,
        cell: Int,
    ) {
        when (kind) {
            Op.U_X_VAR -> {
                heap[cell] = ref(cell)
                varInfo[cell] = b
                regs[a] = ref(cell)
            }
            Op.U_Y_VAR -> {
                heap[cell] = ref(cell)
                varInfo[cell] = b
                local[frame + FRAME + a] = ref(cell)
            }
            Op.U_X_VAL -> heap[cell] = regs[a]
            Op.U_Y_VAL -> heap[cell] = local[frame + FRAME + a]
            Op.U_CONST -> heap[cell] = a
            else -> {
                heap[cell] = ref(cell)
                varInfo[cell] = 0
            }
        }
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

    fun pushVoids(n: Int) {
        val h = top
        if (h + n > heap.size) grow(h + n)
        top = h + n
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
                    execute(cpBase)
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

    // catch(Goal, Catcher, Recovery): a choice point the unwinding of a ball stops at, then the goal followed by
    // '$catch_exit'(N), which ends the catch when the goal exits (its slot 12 is N, its slot 7 whether it is inactive)
    private fun catch(): Int {
        val number = ++catches
        pushChoicePoint(CATCH, 0, 3, 0, continuation, number)
        cutBarrier = cpTop
        val goal = deref(regs[0])
        // (an unbound goal is an instantiation error, raised by callTerm, with the catch active)
        if (tag(goal) == REF) return callTerm(goal)
        val h = top
        if (h + 5 > heap.size) grow(h + 5)
        top = h + 5
        heap[h] = wCatchExit
        heap[h + 1] = program.intWord(number.toLong())
        heap[h + 2] = wComma
        heap[h + 3] = goal
        heap[h + 4] = word(STR, h)
        return callTerm(word(STR, h + 2))
    }

    // The goal of the catch numbered regs[0] exited: if it left no choice point the catch goes; otherwise it stays,
    // inactive, under a CATCH_EXIT choice point that makes it active again when backtracking goes back into the goal
    private fun catchExit(): Int {
        settle()
        val number = intValue(deref(regs[0])).toInt()
        var cp = cpTop - 1
        while (cp >= 0 && !(cps[cp * CP_SIZE + 6] == CATCH && cps[cp * CP_SIZE + 12] == number)) cp--
        if (cp < 0) return CONTINUE
        if (cp == cpTop - 1) {
            popChoicePoint()
        } else {
            cps[cp * CP_SIZE + 7] = 1
            pushChoicePoint(CATCH_EXIT, cp, 0, 0, continuation)
        }
        return CONTINUE
    }

    // Unwinds the choice points above [cpBase] to the nearest catch/3 whose catcher unifies with the ball of
    // [raised], and runs its recovery: UNCAUGHT when there is none, EXHAUSTED when the recovery fails with no choice
    // point left above [cpBase], RESUMED otherwise
    private fun unwind(
        raised: Raised,
        cpBase: Int,
    ): Int {
        if (delayedNext >= 0) dropDelayed()
        while (cpTop > cpBase) {
            val base = (cpTop - 1) * CP_SIZE
            if (cps[base + 6] != CATCH || cps[base + 7] != 0) {
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
            BIG -> if (payload(w) < 0) throw NotNative() else program.bigs[payload(w)]
            STR -> {
                val a = payload(w)
                when (payload(heap[a])) {
                    fPlus -> {
                        val x = eval(heap[a + 1])
                        val y = eval(heap[a + 2])
                        (x + y).also { if (additionOverflows(x, y, it)) throw NotNative() }
                    }
                    fMinus -> {
                        val x = eval(heap[a + 1])
                        val y = eval(heap[a + 2])
                        (x - y).also { if (subtractionOverflows(x, y, it)) throw NotNative() }
                    }
                    fTimes -> {
                        val x = eval(heap[a + 1])
                        val y = eval(heap[a + 2])
                        (x * y).also { if (productOverflows(x, y, it)) throw NotNative() }
                    }
                    fIntDiv -> {
                        val divisor = eval(heap[a + 2])
                        val dividend = eval(heap[a + 1])
                        if (divisor == 0L || (divisor == -1L && dividend == Long.MIN_VALUE)) throw NotNative()
                        dividend / divisor
                    }
                    fMod -> {
                        val divisor = eval(heap[a + 2])
                        if (divisor == 0L) throw NotNative()
                        eval(heap[a + 1]).mod(divisor)
                    }
                    fNeg -> eval(heap[a + 1]).let { if (it == Long.MIN_VALUE) throw NotNative() else -it }
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
        val pinnedResult = pinWord(result)
        solutions(template, goal, Int.MAX_VALUE)
        val resultWord = pinnedWords[pinnedResult]
        pinnedWordsTop = pinnedResult
        return if (unify(resultWord, collectedList)) CONTINUE else FAILED
    }

    // The list the last nested run collected (see solutions)
    private var collectedList = 0

    // The solutions of [goal] in a nested run, up to [limit]: the instances of [template] (NONE for none) collected
    // as a list left in [collectedList]; how many solutions were found
    private fun solutions(
        template: Int,
        goal: Int,
        limit: Int,
    ): Int {
        val pinnedBase = pinnedWordsTop
        val pinnedTemplate = if (template == NONE) NONE else pinWord(template)
        // The barrier choice point: the state before the goal, resumed at the continuation of the call
        pushChoicePoint(BARRIER, 0, 0, 0, continuation)
        val cpBase = cpTop
        continuation = stopAddress
        cutBarrier = cpTop
        var found =
            limit > 0 &&
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
        // The instances go on the bag of the machine, above those of the runs this one is nested in, addressed from
        // [bagBase]: what the nested runs of the goal leave there is gone by the next instance
        val bagBase = copier.top
        val rootsBase = rootsTop
        var count = 0
        try {
            while (found) {
                if (pinnedTemplate != NONE) {
                    val root = copier.copy(deref(pinnedWords[pinnedTemplate]))
                    if (rootsTop == roots.size) roots = roots.copyOf(rootsTop * 2)
                    roots[rootsTop++] = root
                }
                count++
                found = count < limit && nested(cpBase) { backtrack(cpBase) && run(cpBase) }
            }
        } catch (raised: Raised) {
            copier.top = bagBase
            rootsTop = rootsBase
            throw raised
        }
        // (the run stopped at the limit leaves choice points above the barrier: cut, then the barrier restores)
        cutTo(cpBase)
        restoreTop()
        popChoicePoint()
        pinnedWordsTop = pinnedBase
        if (pinnedTemplate == NONE) {
            collectedList = program.emptyList
            rootsTop = rootsBase
            return count
        }
        // The bag onto the heap, its addresses shifted
        val shift = copier.toHeap(bagBase)
        var list = program.emptyList
        for (k in count - 1 downTo 0) {
            val cell = alloc(2)
            val r = roots[rootsBase + k]
            heap[cell] = if (tag(r) == REF || tag(r) == STR || tag(r) == LIST) word(tag(r), payload(r) + shift) else r
            heap[cell + 1] = list
            list = word(LIST, cell)
        }
        collectedList = list
        copier.top = bagBase
        rootsTop = rootsBase
        return count
    }

    override fun findall(
        template: Int,
        goal: Int,
    ): Int {
        val count = nestedForNative(template, goal, Int.MAX_VALUE)
        return if (count >= 0) collectedList else collectedList
    }

    override fun countSolutions(
        goal: Int,
        limit: Int,
    ): Int = nestedForNative(NONE, goal, limit)

    // A nested run on behalf of the native call at hand: its registers and its state kept across the run (the
    // natives the run calls have theirs)
    private fun nestedForNative(
        template: Int,
        goal: Int,
        limit: Int,
    ): Int {
        val arity = nativeArity
        val functor = nativeFunctor
        val names = nativeNames
        val fresh = nativeFresh
        val pinnedBase = pinnedWordsTop
        for (i in 0 until arity) pinWord(regs[i])
        if (names != null) heldNames.add(names)
        if (fresh != null) heldFresh.add(fresh)
        val count =
            try {
                solutions(template, goal, limit)
            } finally {
                if (names != null) heldNames.removeAt(heldNames.size - 1)
                if (fresh != null) heldFresh.removeAt(heldFresh.size - 1)
            }
        for (i in 0 until arity) regs[i] = pinnedWords[pinnedBase + i]
        pinnedWordsTop = pinnedBase
        nativeArity = arity
        nativeFunctor = functor
        nativeNames = names
        nativeFresh = fresh
        // (merged again: a collection in the nested run may have dropped names, and more may come to the same number)
        nativeMerged = 0
        return count
    }

    /** Whether the two unbound cells hold one variable: those read as the same variable of a term given to the machine */
    fun sameVariable(
        a: Int,
        b: Int,
    ): Boolean {
        if (a == b) return true
        val x = varInfo[a]
        val y = varInfo[b]
        return x < 0 && y < 0 && varObjects[-x - 1] == varObjects[-y - 1]
    }

    // The order of two atoms, as tuProlog's (a collation): each pair computed once
    // The atoms ordered so far by the standard order: their ranks by atom (0 for one not met yet), the atoms of each
    // rank. Two atoms with the same rank are equal for the order. An atom met costs a binary search among them
    private var atomRanks = IntArray(64)
    private var rankedAtoms = IntArray(64)
    private var ranks = 0
    private var metAtoms = IntArray(64)
    private var met = 0

    private fun meet(
        atom: Int,
        rank: Int,
    ): Int {
        atomRanks[atom] = rank
        if (met == metAtoms.size) metAtoms = metAtoms.copyOf(met * 2)
        metAtoms[met++] = atom
        return rank
    }

    private fun rankOf(atom: Int): Int {
        if (atom >= atomRanks.size) atomRanks = atomRanks.copyOf(maxOf(atom + 1, atomRanks.size * 2))
        val known = atomRanks[atom]
        if (known != 0) return known
        val name = program.atoms[atom]
        var low = 0
        var high = ranks - 1
        while (low <= high) {
            val middle = (low + high) ushr 1
            val order = compareAtomNames(name, program.atoms[rankedAtoms[middle]])
            when {
                order == 0 -> return meet(atom, middle + 1)
                order < 0 -> high = middle - 1
                else -> low = middle + 1
            }
        }
        // Below the others from [low] on, whose ranks move up
        if (ranks == rankedAtoms.size) rankedAtoms = rankedAtoms.copyOf(ranks * 2)
        rankedAtoms.copyInto(rankedAtoms, low + 1, low, ranks)
        rankedAtoms[low] = atom
        ranks++
        for (i in 0 until met) if (atomRanks[metAtoms[i]] > low) atomRanks[metAtoms[i]]++
        return meet(atom, low + 1)
    }

    // The atoms a sort meets for the first time: above a few, ranked all at once (rankBatch), one sort of their names and
    // one merge with the atoms ranked so far, rather than each by a binary search that renumbers the atoms met before it
    private var unranked = IntArray(64)
    private var unrankedTop = 0

    // The atoms of [w0] with no rank, collected (their rank -1 until rankBatch)
    private fun collectUnranked(w0: Int) {
        var w = deref(w0)
        while (true) {
            when (tag(w)) {
                ATOM -> {
                    val atom = payload(w)
                    if (atom >= atomRanks.size) atomRanks = atomRanks.copyOf(maxOf(atom + 1, atomRanks.size * 2))
                    if (atomRanks[atom] == 0) {
                        atomRanks[atom] = -1
                        if (unrankedTop == unranked.size) unranked = unranked.copyOf(unrankedTop * 2)
                        unranked[unrankedTop++] = atom
                    }
                    return
                }
                STR -> {
                    val a = payload(w)
                    val n = program.arity(heap[a])
                    for (i in 1 until n) collectUnranked(heap[a + i])
                    w = deref(heap[a + n])
                }
                LIST -> {
                    val a = payload(w)
                    collectUnranked(heap[a])
                    w = deref(heap[a + 1])
                }
                else -> return
            }
        }
    }

    // The atoms collectUnranked collected, ranked: the same ranks rankOf gives them one by one, up to a renumbering of all
    // the ranks that keeps their order (the ranks are only compared). Below a few, none is ranked here
    private fun rankBatch() {
        val count = unrankedTop
        unrankedTop = 0
        if (count == 0) return
        if (count < RANKED_ONE_BY_ONE) {
            // (left to rankOf, which ranks only the atoms a comparison reaches)
            for (i in 0 until count) atomRanks[unranked[i]] = 0
            return
        }
        val names = program.atoms
        val fresh = unranked.copyOf(count).sortedWith { x, y -> compareAtomNames(names[x], names[y]) }
        val merged = IntArray(ranks + count)
        val oldToNew = IntArray(ranks + 1)
        val freshRanks = IntArray(count)
        var m = 0
        var i = 0
        var j = 0
        while (i < ranks || j < count) {
            val order =
                when {
                    i == ranks -> 1
                    j == count -> -1
                    else -> compareAtomNames(names[rankedAtoms[i]], names[fresh[j]])
                }
            if (order <= 0) {
                merged[m++] = rankedAtoms[i]
                oldToNew[++i] = m
            } else {
                // (a name equal to the last one ranked takes its rank)
                if (m == 0 || compareAtomNames(names[merged[m - 1]], names[fresh[j]]) != 0) merged[m++] = fresh[j]
                freshRanks[j++] = m
            }
        }
        for (k in 0 until met) atomRanks[metAtoms[k]] = oldToNew[atomRanks[metAtoms[k]]]
        rankedAtoms = merged
        ranks = m
        for (k in 0 until count) meet(fresh[k], freshRanks[k])
    }

    /** The standard order of the atoms [a] and [b] */
    fun atomOrder(
        a: Int,
        b: Int,
    ): Int {
        rankOf(a)
        rankOf(b)
        // (the rank of the first may have moved up when the second was met)
        return atomRanks[a].compareTo(atomRanks[b])
    }

    /** A copy of the term [w0] on the heap, its unbound variables fresh and unnamed */
    fun copyOnHeap(w0: Int): Int {
        val bagBase = copier.top
        val root = copier.copy(w0)
        val shift = copier.toHeap(bagBase, named = false)
        copier.top = bagBase
        return if (tag(root) == REF || tag(root) == STR || tag(root) == LIST) word(tag(root), payload(root) + shift) else root
    }

    override fun sorted(items: IntArray): IntArray? {
        var integers = true
        for (i in items.indices) {
            val item = deref(items[i])
            if (tag(item) != INT) {
                integers = false
                break
            }
        }
        if (integers) {
            // (the words of the small integers are ordered as they are)
            val sorted = IntArray(items.size) { deref(items[it]) }
            sorted.sort()
            return sorted
        }
        for (item in items) if (!StdlibNatives.ground(this, item)) return null
        for (item in items) collectUnranked(item)
        rankBatch()
        val sorted = items.copyOf()
        if (!inOrderAlready(sorted)) sortStably(sorted)
        return sorted
    }

    // Whether [items] end up in the standard order by a single pass: they are in it, or in the reverse of it (the lists
    // arg2p sorts mostly are, being read from a store newest first)
    private fun inOrderAlready(items: IntArray): Boolean {
        if (items.size < 2) return true
        var i = 1
        while (i < items.size && StdlibNatives.order(this, items[i - 1], items[i]) <= 0) i++
        if (i == items.size) return true
        if (i > 1) return false
        i = 1
        while (i < items.size && StdlibNatives.order(this, items[i - 1], items[i]) > 0) i++
        if (i < items.size) return false
        items.reverse()
        return true
    }

    // In the standard order, keeping the order of the equal ones: by insertion when few, merging otherwise
    private fun sortStably(items: IntArray) {
        if (items.size <= 12) {
            for (i in 1 until items.size) {
                val item = items[i]
                var at = i - 1
                while (at >= 0 && StdlibNatives.order(this, items[at], item) > 0) {
                    items[at + 1] = items[at]
                    at--
                }
                items[at + 1] = item
            }
            return
        }
        val left = items.copyOfRange(0, items.size / 2)
        val right = items.copyOfRange(items.size / 2, items.size)
        sortStably(left)
        sortStably(right)
        if (StdlibNatives.order(this, left[left.size - 1], right[0]) <= 0) {
            left.copyInto(items)
            right.copyInto(items, left.size)
            return
        }
        var l = 0
        var r = 0
        for (i in items.indices) {
            items[i] =
                if (r == right.size || (l < left.size && StdlibNatives.order(this, left[l], right[r]) <= 0)) left[l++] else right[r++]
        }
    }

    override fun sortedDistinct(items: IntArray): IntArray? {
        val sorted = sorted(items) ?: return null
        var n = 0
        for (i in sorted.indices) {
            if (n == 0 || StdlibNatives.order(this, sorted[n - 1], sorted[i]) != 0) sorted[n++] = sorted[i]
        }
        return sorted.copyOf(n)
    }

    override val recordProgram: Program get() = program

    override fun int(value: Int): Int = program.smallWord(value)

    // The words of the key of a record, as Flattened reads them: of no use past the lookup at hand
    private var keyWords = IntArray(256)
    private var keySize = 0
    private val keyVariables = IntArray(64)
    private var keyVariablesTop = 0

    override fun recordKey(w: Int): RecordKey? {
        keySize = 0
        keyVariablesTop = 0
        return if (flattened(w)) RecordKey(keyWords, keySize) else null
    }

    private fun pushKey(w: Int) {
        if (keySize == keyWords.size) keyWords = keyWords.copyOf(keySize * 2)
        keyWords[keySize++] = w
    }

    // False for more variables than kept: the record then says
    private fun flattened(w0: Int): Boolean {
        var w = dereferenced(w0)
        while (true) {
            when (tag(w)) {
                REF -> {
                    val address = payload(w)
                    var i = 0
                    while (i < keyVariablesTop && keyVariables[i] != address) i++
                    if (i == keyVariablesTop) {
                        if (i == keyVariables.size) return false
                        keyVariables[keyVariablesTop++] = address
                    }
                    pushKey(ref(i))
                    return true
                }
                STR -> {
                    val a = payload(w)
                    val f = heap[a]
                    pushKey(f)
                    val n = program.arity(f)
                    for (i in 1 until n) if (!flattened(heap[a + i])) return false
                    w = dereferenced(heap[a + n])
                }
                LIST -> {
                    val a = payload(w)
                    pushKey(Program.LIST_KEY)
                    if (!flattened(heap[a])) return false
                    w = dereferenced(heap[a + 1])
                }
                else -> {
                    pushKey(w)
                    return true
                }
            }
        }
    }

    override fun record(w: Int): Record {
        val bagBase = copier.top
        val root = copier.copy(w)
        val words = copier.relativeTo(bagBase)
        copier.top = bagBase
        return Record(
            program,
            words,
            if (tag(root) == REF ||
                tag(root) == STR ||
                tag(root) == LIST
            ) {
                word(tag(root), payload(root) - bagBase)
            } else {
                root
            },
            null,
        )
    }

    override fun recall(record: Record): Int {
        if (record.program !== program) return fromTerm(record.term, HashMap())
        val words = record.words
        val n = words.size
        val base = alloc(n)
        for (i in 0 until n) {
            val x = words[i]
            heap[base + i] = if (tag(x) == REF || tag(x) == STR || tag(x) == LIST) word(tag(x), payload(x) + base) else x
            varInfo[base + i] = 0
        }
        val r = record.root
        if (record.functor != 0) heap[base + payload(r)] = record.functor
        return if (tag(r) == REF || tag(r) == STR || tag(r) == LIST) word(tag(r), payload(r) + base) else r
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
    // The terms copied off the heap: findall's instances and the records. A bag with its addresses, as a stack: a copy
    // goes above what is there, and whoever made it takes it away. A copy is made in one go, nothing runs meanwhile
    private val copier = Copier()

    private inner class Copier {
        var bag = IntArray(256)
        var bagInfo = IntArray(256)
        var top = 0

        // The variables met in the copy at hand: addresses on the heap and in the bag, in a table as they are few
        private val seen = IntArray(64)
        private var seenTop = 0
        private var seenMany: HashMap<Int, Int>? = null

        private fun allocBag(cells: Int): Int {
            if (top + cells > bag.size) {
                bag = bag.copyOf(maxOf(bag.size * 2, top + cells))
                bagInfo = bagInfo.copyOf(bag.size)
            }
            val start = top
            top += cells
            return start
        }

        private fun variable(address: Int): Int {
            val many = seenMany
            if (many != null) {
                many[address]?.let { return it }
            } else {
                var i = 0
                while (i < seenTop) {
                    if (seen[i] == address) return seen[i + 1]
                    i += 2
                }
            }
            val a = allocBag(1)
            bag[a] = ref(a)
            bagInfo[a] = varInfo[address]
            if (many != null) {
                many[address] = a
            } else if (seenTop < seen.size) {
                seen[seenTop++] = address
                seen[seenTop++] = a
            } else {
                // too many to look through
                val all = HashMap<Int, Int>()
                var i = 0
                while (i < seenTop) {
                    all[seen[i]] = seen[i + 1]
                    i += 2
                }
                all[address] = a
                seenMany = all
            }
            return a
        }

        /** What is above [from] on the heap: how much to add to its addresses */
        fun toHeap(
            from: Int,
            named: Boolean = true,
        ): Int {
            val n = top - from
            val base = alloc(n)
            val shift = base - from
            for (i in 0 until n) {
                val w = bag[from + i]
                val t = tag(w)
                heap[base + i] = if (t == REF || t == STR || t == LIST) word(t, payload(w) + shift) else w
                varInfo[base + i] = if (named && t == REF && payload(w) == from + i) bagInfo[from + i] else 0
            }
            return shift
        }

        /** What is above [from], addressed from there */
        fun relativeTo(from: Int): IntArray {
            val n = top - from
            return IntArray(n) { i ->
                val w = bag[from + i]
                val t = tag(w)
                if (t == REF || t == STR || t == LIST) word(t, payload(w) - from) else w
            }
        }

        /** A copy of the term [w0] on the bag: its word, with the addresses of the bag */
        fun copy(w0: Int): Int {
            seenTop = 0
            seenMany = null
            return copyTerm(w0)
        }

        private fun copyTerm(w0: Int): Int {
            val w = deref(w0)
            return when (tag(w)) {
                REF -> ref(variable(payload(w)))
                STR -> {
                    val a = payload(w)
                    val n = program.arity(heap[a])
                    val s = allocBag(n + 1)
                    bag[s] = heap[a]
                    for (i in 1..n) {
                        val arg = copyTerm(heap[a + i])
                        bag[s + i] = arg
                    }
                    word(STR, s)
                }
                LIST -> {
                    val first = allocBag(2)
                    var s = first
                    var a = payload(w)
                    while (true) {
                        val head = copyTerm(heap[a])
                        bag[s] = head
                        val t = deref(heap[a + 1])
                        if (tag(t) == LIST) {
                            val s2 = allocBag(2)
                            bag[s + 1] = word(LIST, s2)
                            s = s2
                            a = payload(t)
                        } else {
                            val tail = copyTerm(t)
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
        nativeFresh = null
        nativeMerged = 0
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

    override fun intValue(w: Int): Long = if (tag(w) == INT) payload(w).toLong() else program.bigInteger(payload(w)).toLong()

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

    override fun unifiable(
        a: Int,
        b: Int,
    ): Boolean {
        // (every binding trailed, then undone)
        val mark = trailTop
        val barrier = heapBarrier
        heapBarrier = Int.MAX_VALUE
        val ok = unify(a, b)
        undoTrail(mark)
        heapBarrier = barrier
        return ok
    }

    override fun identical(
        a: Int,
        b: Int,
    ): Boolean = StdlibNatives.identical(this, a, b)

    override fun hashOf(w: Int): Int = StdlibNatives.hash(this, w, 17)

    override fun variable(): Int = ref(newVariable())

    override fun term(w: Int): Term = toTerm(w, nativeNames ?: HashMap<Int, Var>().also { nativeNames = it })

    override fun word(term: Term): Int {
        val names = nativeNames
        val cells = nativeFresh ?: HashMap<Var, Int>().also { nativeFresh = it }
        if (names != null && names.size != nativeMerged) {
            for ((cell, variable) in names) cells[variable] = cell
            nativeMerged = names.size
        }
        return fromTerm(term, cells)
    }

    override val context: ExecutionContext get() = PrologExecutionContext(environmentSeen())

    override val libraries: Runtime get() = scope.libraries ?: environment!!.libraries

    override fun libraryValue(compute: (Runtime) -> Any?): Any? {
        val scope = scope
        val libraries = scope.libraries ?: return compute(environment!!.libraries)
        if (!scope.libraryValueKnown) {
            scope.libraryValue = compute(libraries)
            scope.libraryValueKnown = true
        }
        return scope.libraryValue
    }

    override val theoryVersion: Long? get() = program.theoryVersion

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

    /** Enters the goal [goal] resolved with the clauses of [store] for it: their heads unified in place */
    fun storeGoal(
        goal: Int,
        store: ClauseStore,
        wrap: GoalWrapper?,
    ): Int {
        store.bind(program)
        val g = deref(goal)
        val candidates = store.candidates(heap, g)
        if (candidates.size == 0) return FAILED
        val arity = loadArguments(g)
        return tryStored(StoredCall(candidates, wrap, false), candidates.size - 1, arity, NONE)
    }

    /** Answers `clause(Head, Body)`, for a callable [head]: the clauses of [store] whose head and body unify with them */
    fun storeClause(
        head: Int,
        body: Int,
        store: ClauseStore,
    ): Int {
        store.bind(program)
        val g = deref(head)
        val candidates = store.candidates(heap, g)
        if (candidates.size == 0) return FAILED
        val arity = loadArguments(g)
        if (arity + 1 > regs.size) regs = regs.copyOf((arity + 1) * 2)
        regs[arity] = body
        return tryStored(StoredCall(candidates, null, true), candidates.size - 1, arity + 1, NONE)
    }

    /**
     * Whether a fact of [store] unifies with [goal] (or, with [body], whether a clause does with `clause(goal, body)`),
     * nothing left bound; null when a rule may answer, whose body is to be solved
     */
    fun storeHolds(
        goal: Int,
        body: Int?,
        store: ClauseStore,
    ): Boolean? {
        store.bind(program)
        val g = deref(goal)
        val candidates = store.candidates(heap, g)
        val items = candidates.items
        val arity = loadArguments(g)
        val savedTop = top
        val savedBarrier = heapBarrier
        val mark = trailTop
        for (i in candidates.size - 1 downTo 0) {
            val record = items[i]!!.record
            if (!headMayMatch(record, arity)) continue
            if (body == null && record.isRule) return null
            heapBarrier = Int.MAX_VALUE
            enterRecord(record)
            val holds = unifyHead(record, arity) && (body == null || unify(body, copyRecord(record.words, record.body)))
            undoTrail(mark)
            heapBarrier = savedBarrier
            top = savedTop
            if (holds) return true
        }
        return false
    }

    /** The facts of [store] that unify with [goal], newest first, nothing left bound and the registers as they were */
    fun storeMatching(
        goal: Int,
        store: ClauseStore,
    ): List<ClauseStore.Stored> {
        store.bind(program)
        val g = deref(goal)
        val candidates = store.candidates(heap, g)
        val items = candidates.items
        val arity =
            when (tag(g)) {
                STR -> program.arity(heap[payload(g)])
                LIST -> 2
                else -> 0
            }
        val saved = regs.copyOf(arity)
        loadArguments(g)
        val savedTop = top
        val savedBarrier = heapBarrier
        val mark = trailTop
        val matching = ArrayList<ClauseStore.Stored>()
        for (i in candidates.size - 1 downTo 0) {
            val stored = items[i]!!
            val record = stored.record
            if (record.isRule || !headMayMatch(record, arity)) continue
            heapBarrier = Int.MAX_VALUE
            enterRecord(record)
            if (unifyHead(record, arity)) matching.add(stored)
            undoTrail(mark)
            heapBarrier = savedBarrier
            top = savedTop
        }
        saved.copyInto(regs)
        return matching
    }

    // Tries the clauses of [call] from position [from] down: past the heads that cannot match, a choice point left if
    // another may. [index] is that of the call among [calls], once it has one
    private fun tryStored(
        call: StoredCall,
        from: Int,
        arity: Int,
        index: Int,
    ): Int {
        val items = call.candidates.items
        val headArity = if (call.asClause) arity - 1 else arity
        var i = from
        while (i >= 0 && !headMayMatch(items[i]!!.record, headArity)) i--
        if (i < 0) return FAILED
        var next = i - 1
        while (next >= 0 && !headMayMatch(items[next]!!.record, headArity)) next--
        if (next >= 0) {
            val at =
                if (index != NONE) {
                    index
                } else {
                    calls.add(call)
                    calls.size - 1
                }
            pushChoicePoint(SOURCED, at, arity, 0, continuation, next)
        }
        val record = items[i]!!.record
        enterRecord(record)
        if (!unifyHead(record, headArity)) return FAILED
        if (call.asClause) return if (unify(regs[headArity], copyRecord(record.words, record.body))) CONTINUE else FAILED
        if (!record.isRule) return CONTINUE
        val body = copyRecord(record.words, record.body)
        val wrap = call.wrap
        cutBarrier = cpTop
        return callTerm(if (wrap == null) body else wrapped(body, wrap))
    }

    // ---- a record against the heap, in place: nothing is copied but what a variable of the heap is bound to

    // The variables of the record at hand, by their relative addresses: their words on the heap, for this stamp
    private var recordVariables = IntArray(64)
    private var recordStamps = IntArray(64)
    private var recordStamp = 0

    private fun enterRecord(record: Record) {
        val n = record.words.size
        if (n > recordStamps.size) {
            recordVariables = IntArray(n * 2)
            recordStamps = IntArray(n * 2)
            recordStamp = 0
        }
        recordStamp++
    }

    // The arguments of the head of [record] against the registers
    private fun unifyHead(
        record: Record,
        arity: Int,
    ): Boolean {
        val words = record.words
        val head = record.head
        val offset = if (tag(head) == LIST) payload(head) else payload(head) + 1
        for (i in 0 until arity) if (!unifyRecord(words, words[offset + i], regs[i])) return false
        return true
    }

    // Whether the head of [record] may unify with the arguments in the registers: all that binds nothing told apart
    private fun headMayMatch(
        record: Record,
        arity: Int,
    ): Boolean {
        val words = record.words
        val head = record.head
        val offset = if (tag(head) == LIST) payload(head) else payload(head) + 1
        for (i in 0 until arity) if (!mayMatch(words, words[offset + i], regs[i])) return false
        return true
    }

    private fun mayMatch(
        words: IntArray,
        r0: Int,
        w0: Int,
    ): Boolean {
        var r = r0
        var w = w0
        while (true) {
            val t = tag(r)
            if (t == REF) return true
            val d = deref(w)
            val td = tag(d)
            if (td == REF) return true
            if (t != td) return false
            when (t) {
                STR -> {
                    val a = payload(r)
                    val b = payload(d)
                    val f = words[a]
                    if (f != heap[b]) return false
                    val n = program.arity(f)
                    for (i in 1 until n) if (!mayMatch(words, words[a + i], heap[b + i])) return false
                    r = words[a + n]
                    w = heap[b + n]
                }
                LIST -> {
                    val a = payload(r)
                    val b = payload(d)
                    if (!mayMatch(words, words[a], heap[b])) return false
                    r = words[a + 1]
                    w = heap[b + 1]
                }
                else -> return r == d
            }
        }
    }

    private fun unifyRecord(
        words: IntArray,
        r0: Int,
        w0: Int,
    ): Boolean {
        var r = r0
        var w = w0
        while (true) {
            when (tag(r)) {
                REF -> {
                    val a = payload(r)
                    if (recordStamps[a] == recordStamp) return unify(recordVariables[a], w)
                    recordStamps[a] = recordStamp
                    recordVariables[a] = w
                    return true
                }
                STR -> {
                    val d = deref(w)
                    when (tag(d)) {
                        REF -> {
                            bind(payload(d), copyRecord(words, r))
                            return true
                        }
                        STR -> {
                            val a = payload(r)
                            val b = payload(d)
                            val f = words[a]
                            if (f != heap[b]) return false
                            val n = program.arity(f)
                            for (i in 1 until n) if (!unifyRecord(words, words[a + i], heap[b + i])) return false
                            r = words[a + n]
                            w = heap[b + n]
                        }
                        else -> return false
                    }
                }
                LIST -> {
                    val d = deref(w)
                    when (tag(d)) {
                        REF -> {
                            bind(payload(d), copyRecord(words, r))
                            return true
                        }
                        LIST -> {
                            val a = payload(r)
                            val b = payload(d)
                            if (!unifyRecord(words, words[a], heap[b])) return false
                            r = words[a + 1]
                            w = heap[b + 1]
                        }
                        else -> return false
                    }
                }
                else -> {
                    val d = deref(w)
                    if (d == r) return true
                    if (tag(d) != REF) return false
                    bind(payload(d), r)
                    return true
                }
            }
        }
    }

    // The term [r] of a record on the heap, its variables those met so far or fresh ones
    private fun copyRecord(
        words: IntArray,
        r: Int,
    ): Int =
        when (tag(r)) {
            REF -> {
                val a = payload(r)
                if (recordStamps[a] == recordStamp) {
                    recordVariables[a]
                } else {
                    val v = ref(newVariable(0))
                    recordStamps[a] = recordStamp
                    recordVariables[a] = v
                    v
                }
            }
            STR -> {
                val a = payload(r)
                val f = words[a]
                val n = program.arity(f)
                val h = alloc(n + 1)
                heap[h] = f
                for (i in 1..n) {
                    val argument = copyRecord(words, words[a + i])
                    heap[h + i] = argument
                }
                word(STR, h)
            }
            LIST -> {
                val first = alloc(2)
                var cell = first
                var a = payload(r)
                while (true) {
                    val item = copyRecord(words, words[a])
                    heap[cell] = item
                    val tail = words[a + 1]
                    if (tag(tail) == LIST) {
                        val next = alloc(2)
                        heap[cell + 1] = word(LIST, next)
                        cell = next
                        a = payload(tail)
                    } else {
                        val last = copyRecord(words, tail)
                        heap[cell + 1] = last
                        break
                    }
                }
                word(LIST, first)
            }
            else -> r
        }

    // The arguments of the goal [g] (dereferenced) in the registers; its arity
    private fun loadArguments(g: Int): Int =
        when (tag(g)) {
            STR -> {
                val a = payload(g)
                val n = program.arity(heap[a])
                if (n > regs.size) regs = regs.copyOf(n * 2)
                for (i in 0 until n) regs[i] = heap[a + 1 + i]
                n
            }
            LIST -> {
                val a = payload(g)
                regs[0] = heap[a]
                regs[1] = heap[a + 1]
                2
            }
            else -> 0
        }

    // The body [w] with each of its goals wrapped by [wrap]: a conjunction rebuilt, `true` left alone
    private fun wrapped(
        w: Int,
        wrap: GoalWrapper,
    ): Int {
        val d = deref(w)
        if (tag(d) == STR && heap[payload(d)] == wComma) {
            val a = payload(d)
            val left = wrapped(heap[a + 1], wrap)
            val right = wrapped(heap[a + 2], wrap)
            val h = alloc(3)
            heap[h] = wComma
            heap[h + 1] = left
            heap[h + 2] = right
            return word(STR, h)
        }
        if (d == wTrue) return d
        return wrap.wrap(this, d)
    }

    override fun inline(
        goal: Int,
        libraries: Runtime,
        theoryVisible: Boolean,
    ): Int = inlineWord(goal, libraries, theoryVisible)

    // A module call of the instructions MODULE_CALL and MODULE_EXECUTE, the arguments of the goal in the registers
    private fun moduleCall(
        kind: Int,
        module: Int,
        functor: Int,
        arity: Int,
    ): Int {
        moduleGoalFunctor = functor
        moduleGoalArity = arity
        val primitive = scope.primitive(kind) as? RegisterGoalPrimitive
        if (primitive == null) {
            // (no such primitive here: the call as a term, for whatever the resolution makes of it)
            val goal = goalOnRegisters()
            regs[0] = module
            regs[1] = goal
            return call(kind, 2)
        }
        nativeFunctor = kind
        nativeArity = 0
        nativeNames = null
        nativeFresh = null
        nativeMerged = 0
        return try {
            primitive.callOnRegisters(this, module)
        } catch (raised: Raised) {
            throw raised
        } catch (halt: Halt) {
            throw halt
        } catch (exception: Exception) {
            throw raised(exception)
        }
    }

    private var moduleGoalFunctor = 0
    private var moduleGoalArity = 0

    override fun goalOnRegisters(): Int {
        val arity = moduleGoalArity
        val functor = moduleGoalFunctor
        if (arity == 0) return word(ATOM, program.functorNames[functor])
        val h = alloc(arity + 1)
        heap[h] = word(FUN, functor)
        for (i in 0 until arity) heap[h + 1 + i] = regs[i]
        return word(STR, h)
    }

    override fun inlineOnRegisters(
        libraries: Runtime,
        theoryVisible: Boolean,
    ): Int {
        allocate(2)
        local[frame + FRAME] = smallInt(scope.id)
        local[frame + FRAME + 1] = smallInt(if (this.theoryVisible) 1 else 0)
        continuation = restoreScopeBlock
        scope = program.scopeOf(libraries)
        if (program.maxRegister + 1 > regs.size) regs = regs.copyOf(program.maxRegister + 1)
        this.theoryVisible = theoryVisible
        cutBarrier = cpTop
        return when (call(moduleGoalFunctor, moduleGoalArity)) {
            FAILED -> FAILED
            CONTINUE -> jump(continuation)
            else -> JUMPED
        }
    }

    override fun alternatives(resume: NativeResume) {
        pushChoicePoint(NATIVE, calls.size, nativeArity, 0, continuation)
        calls.add(NativeAlternatives(resume, nativeNames))
    }

    // The alternatives of a native call, with the variables its terms gave the cells (the same on resumption, for
    // the cells still there)
    private class NativeAlternatives(
        val resume: NativeResume,
        val names: HashMap<Int, Var>?,
    )

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
    // The goal [goal] entered against [libraries] (the theory of the user visible or not), the scope of the caller
    // restored by the frame allocated here when the goal returns
    private fun inlineWord(
        goal: Int,
        libraries: Runtime,
        theory: Boolean,
    ): Int {
        allocate(2)
        local[frame + FRAME] = smallInt(scope.id)
        local[frame + FRAME + 1] = smallInt(if (theoryVisible) 1 else 0)
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
        val blocks = (top + 31) ushr 5
        if (marks.size < blocks) marks = IntArray(maxOf(blocks, (heap.size + 31) ushr 5))
        marks.fill(0, 0, blocks)
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
        relocateNativeCells()
        for (cp in 0 until cpTop) {
            val base = cp * CP_SIZE
            cps[base + 1] = newAddress(cps[base + 1])
        }
        // The cells kept slide down, their words relocated, their names with them
        var f = 0
        for (b in 0 until blocks) {
            var bits = marks[b]
            val base = b shl 5
            while (bits != 0) {
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

    // The cells the variables of native calls were given, each map once: what no root keeps is gone
    private fun namesBelowTop(names: HashMap<Int, Var>): HashMap<Int, Var> {
        val below = HashMap<Int, Var>(names.size * 2)
        for ((cell, variable) in names) if (cell < top) below[cell] = variable
        return below
    }

    private fun relocateNativeCells() {
        nativeMerged = 0
        val named = ArrayList<HashMap<Int, Var>>()
        val keep = { names: HashMap<Int, Var>? -> if (names != null && named.none { it === names }) named.add(names) }
        keep(nativeNames)
        heldNames.forEach { keep(it) }
        for (call in calls) if (call is NativeAlternatives) keep(call.names)
        for (names in named) {
            val entries = names.entries.map { it.key to it.value }
            names.clear()
            for ((cell, variable) in entries) if (cell < collectedFrom && marked(cell)) names[newAddress(cell)] = variable
        }
        val freshes = ArrayList<HashMap<Var, Int>>()
        val keepFresh = { fresh: HashMap<Var, Int>? -> if (fresh != null && freshes.none { it === fresh }) freshes.add(fresh) }
        keepFresh(nativeFresh)
        heldFresh.forEach { keepFresh(it) }
        for (fresh in freshes) {
            val entries = fresh.entries.map { it.key to it.value }
            fresh.clear()
            for ((variable, cell) in entries) if (cell < collectedFrom && marked(cell)) fresh[variable] = newAddress(cell)
        }
    }

    // The slots of the frames from [start] up (each once)
    private fun markFrames(start: Int) {
        var e = start
        while (e != NONE && !frameVisited[e]) {
            frameVisited[e] = true
            val slots = local[e + 3]
            for (i in 0 until slots) markSlot(local[e + FRAME + i])
            e = local[e]
        }
    }

    private fun relocateFrames(start: Int) {
        var e = start
        while (e != NONE && !frameVisited[e]) {
            frameVisited[e] = true
            val slots = local[e + 3]
            for (i in 0 until slots) local[e + FRAME + i] = relocatedSlot(local[e + FRAME + i])
            e = local[e]
        }
    }

    // A slot may hold what a retry left behind, never to be read: an address above the top, or one whose cell has been
    // reused since. What it finds there is not followed if it is no term, and the slot is cleared if nothing else keeps it
    private fun markSlot(w: Int) {
        val a = payload(w)
        when (tag(w)) {
            REF -> if (a < top) markWord(w)
            STR -> if (a < top && tag(heap[a]) == FUN) markWord(w)
            LIST -> if (a + 1 < top) markWord(w)
            else -> Unit
        }
    }

    private fun relocatedSlot(w: Int): Int {
        val t = tag(w)
        if (t != REF && t != STR && t != LIST) return w
        val a = payload(w)
        return if (a >= collectedFrom || !marked(a)) UNSET else relocated(w)
    }

    private fun marked(c: Int): Boolean = (marks[c ushr 5] ushr (c and 31)) and 1 != 0

    private fun mark(c: Int) {
        marks[c ushr 5] = marks[c ushr 5] or (1 shl (c and 31))
    }

    // The new address of a cell kept, or of a mark [address] (the first cell at or after it kept): the cells kept
    // below it
    private fun newAddress(address: Int): Int {
        if (address >= collectedFrom) return collectedTo
        val b = address ushr 5
        return counts[b] + (marks[b] and ((1 shl (address and 31)) - 1)).countOneBits()
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
            is Integer -> program.integerWord(term.value)
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
            ATOM -> program.atomTerm(payload(w))
            INT -> program.integerTerm(payload(w))
            BIG -> Integer.of(program.bigInteger(payload(w)))
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
                CATCH -> if (cps[cp * CP_SIZE + 7] == 0) "catch" else "catch (inactive)"
                CATCH_EXIT -> "catch exit"
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
        delayedNext = -1
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

        // Above the choice points a catch/3 goal left when it exited: backtracking to it makes the catch active again
        const val CATCH_EXIT = -8

        // Where the clauses of a choice point come from: the libraries and the theory, or the theory alone
        private const val MERGED = 0
        private const val THEORY = 1

        // (as [MERGED], the arguments of the goal beyond the first all unbound)
        private const val MERGED_FREE = 2

        // Fewer atoms new to a sort than this are left to be ranked one by one as compared (see rankBatch)
        private const val RANKED_ONE_BY_ONE = 32

        // The outcomes of a call
        const val FAILED = 0
        const val CONTINUE = 1
        const val JUMPED = 2

        // An inline builtin leaving the call to the builtin itself
        private const val NOT_INLINE = -1

        // The outcomes of unwinding a ball
        private const val UNCAUGHT = 0
        private const val RESUMED = 1

        // (no value of evaluateSmall: Int.MIN_VALUE as a value only takes the longer way)
        private const val NOT_SMALL = Int.MIN_VALUE

        // Up to this, a product fits in an Int
        private const val SMALL_FACTOR = 46340
        private const val EXHAUSTED = 2

        // The heap cells above which a call collects
        const val GC_MIN = 1 shl 20

        // The calls between two looks at the clock
        private const val CHECK_EVERY = 1024

        /** The message of the halt of a resolution its caller interrupted */
        const val INTERRUPTED = "The resolution was interrupted by its caller"
    }
}

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
