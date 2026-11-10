package it.unibo.tuprolog.argumentation.engine.vm

import it.unibo.tuprolog.argumentation.engine.vm.NativeCall.Companion.CONTINUE
import it.unibo.tuprolog.argumentation.engine.vm.NativeCall.Companion.FAIL
import it.unibo.tuprolog.argumentation.engine.vm.NativeCall.Companion.JUMPED
import it.unibo.tuprolog.argumentation.engine.words.Words
import it.unibo.tuprolog.argumentation.engine.words.Words.ATOM
import it.unibo.tuprolog.argumentation.engine.words.Words.BIG
import it.unibo.tuprolog.argumentation.engine.words.Words.FLT
import it.unibo.tuprolog.argumentation.engine.words.Words.FUN
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
import it.unibo.tuprolog.core.Atom
import it.unibo.tuprolog.core.Indicator
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.Var
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.currentTimeInstant
import it.unibo.tuprolog.solve.exception.LogicError
import it.unibo.tuprolog.solve.exception.ResolutionException
import it.unibo.tuprolog.solve.exception.TimeOutException
import it.unibo.tuprolog.solve.exception.error.MessageError
import it.unibo.tuprolog.solve.exception.error.SystemError
import it.unibo.tuprolog.solve.exception.warning.MissingPredicate
import it.unibo.tuprolog.solve.flags.Unknown
import it.unibo.tuprolog.solve.library.Runtime
import it.unibo.tuprolog.solve.primitive.Primitive
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
 */
internal class Machine(
    val program: Program,
    /** The environment the primitives of the libraries are called in, as their side effects left it */
    var environment: ExecutionEnvironment,
    // The heap cells above which a call collects (the threshold then follows the cells kept)
    @JvmField
    val gcMinimum: Int = GC_MIN,
) : MachineCall {
    private val pooled = MachineArrays.take()

    @JvmField
    var heap = pooled.heap

    @JvmField
    var top = 0

    // What a cell holding an unbound variable is called when the term is read: 0 nothing, a name atom and one for a
    // variable the code made (fresh as read), minus an index and one for the variable object the cell was made for
    // (the same object as read again: a stored term read twice gives the same variables)
    @JvmField
    var varInfo = pooled.varInfo
    private val varObjects = ArrayList<Var>()

    // The cells of the variables of the query (the first ones), and when each was bound (the bindings of a solution are
    // reported in the order they were made)
    private var queryCells = 0
    private var queryBound = IntArray(16)
    private var bindings = 0

    // The cells below this address are trailed when bound (the heap top at the latest choice point)
    @JvmField
    var heapBarrier = 0

    @JvmField
    var trail = pooled.trail

    @JvmField
    var trailTop = 0

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
    private var cpTables = arrayOfNulls<ClauseTable>(cps.size / CP_SIZE)
    private var cpCandidates = arrayOfNulls<IntArray>(cps.size / CP_SIZE)
    var cpTop = 0
        private set

    @JvmField
    var cpArgs = pooled.cpArgs

    @JvmField
    var cpArgsTop = 0

    /** The registers: the arguments of the goal being called, then the temporary variables of the clause */
    @JvmField
    var regs = IntArray(maxOf(64, program.maxRegister + 1))

    private var pdl = IntArray(1 shl 10)

    // The words the code around a nested run holds: relocated by the collector
    @JvmField
    var pinnedWords = IntArray(16)

    @JvmField
    var pinnedWordsTop = 0

    // The parts of the machine: the terms copied off the heap, the order of terms, the lookups of the stores, the primitives
    // that are not native, the collector of the heap
    private val copier = TermCopier(this)

    @JvmField
    val termOrder = TermOrder(this)
    private val stores = StoreLookup(this)
    private val primitives = PrimitiveFacade(this)
    private val collector = HeapCollector(this)

    // The builtins by functor (for each program: a machine is made for each solve)
    private val builtinTable = program.builtins { builtinsOf(program) }

    // The time the run must end by, and the calls made since it was last checked
    private var deadline = Long.MAX_VALUE
    private var callsSinceCheck = 0

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

    // The arity of the native primitive being called, and the variables its [term]s gave the cells (one call)
    private var nativeArity = 0

    @JvmField
    var nativeNames: HashMap<Int, Var>? = null

    // The cells the words of a native call gave the variables of its terms that were not cells already (fresh ones
    // of a clause copied, say): the same variable the same cell within the call
    @JvmField
    var nativeFresh: HashMap<Var, Int>? = null

    // How many of [nativeNames] are in [nativeFresh] already
    @JvmField
    var nativeMerged = 0

    // Those of the native calls a nested run is under, for a collection to find
    @JvmField
    val heldNames = ArrayList<HashMap<Int, Var>>()

    @JvmField
    val heldFresh = ArrayList<HashMap<Var, Int>>()

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
    private val comparisonFunctors = IntArray(Op.C_NE + 1).also { for ((name, kind) in Op.COMPARISONS) it[kind] = program.functor(name, 2) }

    // The stacks of the inline arithmetic: the values being computed, the words of an expression being built for the
    // builtin when the machine cannot compute it, and whether the last evaluation could not
    private var values = LongArray(32)
    private val smallValues = IntArray(32)
    private var exprWords = IntArray(32)

    // The instances of the runs of findall, those of the nested ones above: where each instance starts in the bag
    private var roots = IntArray(256)
    private var rootsTop = 0
    private var evalFailed = false

    @JvmField
    val wComma = program.functorWord(",", 2)
    private val wCatchExit = program.functorWord("\$catch_exit", 1)

    // The number of the last catch/3 run, telling its choice point apart (see catchExit)
    private var catches = 0
    private val fList = program.functor(".", 2)

    // The code of the control constructs called as goals at run time, of the end of a run, of the return from an
    // inlined goal
    private val controlBlocks = program.controlBlocks()
    private val stopAddress = controlBlocks.stop
    private val conjunctionBlock = controlBlocks.conjunction
    private val disjunctionBlock = controlBlocks.disjunction
    private val ifThenElseBlock = controlBlocks.ifThenElse
    private val negationBlock = controlBlocks.negation
    private val restoreScopeBlock = controlBlocks.restoreScope
    private val fArg = program.functor("arg", 3)
    private val fFunctor = program.functor("functor", 3)

    // An expression the machine does not evaluate itself (see builtinsOf)
    internal class NotNative : RuntimeException()

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

    // ---- heap

    /** The word of the integer [value]: a small one as it is, another a compound on the heap (see [Numbers]) */
    fun intWord(value: Long): Int = Numbers.write(program, value, { alloc(it) }, { a, w -> heap[a] = w })

    // The word of the sum of two small integers
    private fun sumWord(
        a: Int,
        b: Int,
    ): Int {
        val sum = a.toLong() + b
        return if (Words.fitsInt(sum)) word(INT, sum.toInt()) else intWord(sum)
    }

    /**
     * The tag a type test reads of the dereferenced word [w]: its own, but [BIG] or [FLT] for the compound of a number no word
     * holds (see [Numbers])
     */
    fun typeTag(w: Int): Int {
        val t = tag(w)
        if (t != STR) return t
        val n = program.numberTag(heap[payload(w)])
        return if (n == 0) STR else n
    }

    // The Long of the integer compound [w] (see Numbers): null if it is no integer, or takes more than 63 bits
    private fun longOfNumber(w: Int): Long? {
        val a = payload(w)
        val f = heap[a]
        if (program.numberTag(f) != BIG) return null
        val n = program.arity(f)
        return Numbers.longOf(payload(deref(heap[a + 1])), IntArray(n - 1) { payload(deref(heap[a + 2 + it])) })
    }

    // The integer of the integer compound [w] (see Numbers)
    fun bigIntegerOfNumber(w: Int): org.gciatto.kt.math.BigInteger {
        val a = payload(w)
        val n = program.arity(heap[a])
        return Numbers.bigOf(payload(deref(heap[a + 1])), IntArray(n - 1) { payload(deref(heap[a + 2 + it])) })
    }

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

    override fun deref(w0: Int): Int {
        var w = w0
        while (tag(w) == REF) {
            val c = heap[payload(w)]
            if (c == w) return w
            w = c
        }
        return w
    }

    fun bind(
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

    fun undoTrail(mark: Int) {
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

    // Two variables met: the first bound to the second (X = Y binds X, as Arg2pTest expects; either way is safe here, both
    // cells being on the heap and the older one trailed)
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

    private fun frameEnd(e: Int): Int = if (e == NONE) 0 else e + FRAME + local[e + FRAME_SLOTS]

    private fun localMark(): Int = if (cpTop == 0) 0 else cps[(cpTop - 1) * CP_SIZE + CP_LOCAL_TOP]

    private fun allocate(slots: Int) {
        val e = maxOf(frameEnd(frame), localMark())
        if (e + FRAME + slots > local.size) local = local.copyOf(maxOf(local.size * 2, e + FRAME + slots))
        local[e] = frame
        local[e + FRAME_CONTINUATION] = continuation
        local[e + FRAME_BARRIER] = cutBarrier
        local[e + FRAME_SLOTS] = slots
        frame = e
    }

    @Suppress("NOTHING_TO_INLINE")
    private inline fun deallocate() {
        continuation = local[frame + FRAME_CONTINUATION]
        cutBarrier = local[frame + FRAME_BARRIER]
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
    private var delayedTable: ClauseTable? = null
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
        table: ClauseTable,
        candidates: IntArray?,
        functor: Int,
        entered: ClauseCode,
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
        cps[base + CP_HEAP_TOP] = delayedTop
        cps[base + CP_LOCAL_TOP] = maxOf(frameEnd(delayedFrame), localMark())
        cps[base + CP_FRAME] = delayedFrame
        cps[base + CP_CONTINUATION] = continuation
        if (table == null) {
            cps[base + CP_RESUME_AT] = delayedNext
            cps[base + CP_KIND] = RESUME
            cps[base + CP_NEXT] = 0
            cps[base + CP_BARRIER] = 0
        } else {
            cps[base + CP_RESUME_AT] = 0
            cps[base + CP_KIND] = delayedFunctor
            cps[base + CP_NEXT] = delayedNext
            cps[base + CP_BARRIER] = cutBarrier
        }
        cps[base + CP_ARGS] = cpArgsTop
        cps[base + CP_CALLS] = calls.size
        cps[base + CP_ARITY] = delayedArity
        cps[base + CP_KEY] = delayedKey
        cps[base + CP_SCOPE] = scope.id
        cps[base + CP_THEORY_VISIBLE] = if (theoryVisible) 1 else 0
        cps[base + CP_PINNED] = pinnedWordsTop
        cps[base + CP_SOURCE] = if (delayedFree && table != null) MERGED_FREE else MERGED
        cpArgsTop += delayedArity
        cpTop++
        delayedNext = -1
    }

    private fun dropDelayed() {
        delayedNext = -1
        heapBarrier = if (cpTop > 0) cps[(cpTop - 1) * CP_SIZE + CP_HEAP_TOP] else 0
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

    fun pushChoicePoint(
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
        cps[base + CP_HEAP_TOP] = top
        cps[base + CP_LOCAL_TOP] = maxOf(frameEnd(frame), localMark())
        cps[base + CP_FRAME] = frame
        cps[base + CP_CONTINUATION] = continuation
        cps[base + CP_RESUME_AT] = resumeAt
        cps[base + CP_KIND] = functor
        cps[base + CP_NEXT] = nextClause
        cps[base + CP_BARRIER] = barrier
        cps[base + CP_ARGS] = cpArgsTop
        cps[base + CP_CALLS] = calls.size
        cps[base + CP_ARITY] = arity
        cps[base + CP_KEY] = key
        cps[base + CP_SCOPE] = scope.id
        cps[base + CP_THEORY_VISIBLE] = if (theoryVisible) 1 else 0
        cps[base + CP_PINNED] = pinnedWordsTop
        cps[base + CP_SOURCE] = source
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
        top = cps[base + CP_HEAP_TOP]
        frame = cps[base + CP_FRAME]
        continuation = cps[base + CP_CONTINUATION]
        pc = cps[base + CP_RESUME_AT]
        if (scope.id != cps[base + CP_SCOPE]) scope = program.scope(cps[base + CP_SCOPE])
        theoryVisible = cps[base + CP_THEORY_VISIBLE] != 0
        pinnedWordsTop = cps[base + CP_PINNED]
    }

    private fun popChoicePoint() {
        cpTop--
        val base = cpTop * CP_SIZE
        cpArgsTop = cps[base + CP_ARGS]
        truncateCalls(cps[base + CP_CALLS])
        heapBarrier = if (cpTop > 0) cps[(cpTop - 1) * CP_SIZE + CP_HEAP_TOP] else 0
    }

    private fun truncateCalls(size: Int) {
        while (calls.size > size) calls.removeAt(calls.size - 1)
    }

    fun cutTo(barrier: Int) {
        if (delayedNext >= 0) dropDelayed()
        if (cpTop > barrier) {
            val base = barrier * CP_SIZE
            cpArgsTop = cps[base + CP_ARGS]
            truncateCalls(cps[base + CP_CALLS])
            cpTop = barrier
            heapBarrier = if (cpTop > 0) cps[(cpTop - 1) * CP_SIZE + CP_HEAP_TOP] else 0
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
            val functor = cps[base + CP_KIND]
            val nextClause = cps[base + CP_NEXT]
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
                    cps[nextClause * CP_SIZE + CP_CATCH_INACTIVE] = 0
                    popChoicePoint()
                }
                PRIMITIVE -> {
                    val call = calls[nextClause] as PrimitiveCall
                    call.pending?.let {
                        popChoicePoint()
                        throw raised(it)
                    }
                    val response = primitives.nextResponse(call.responses)
                    if (!primitives.hasNextResponse(call)) popChoicePoint()
                    if (primitives.applyResponse(response, call)) return true
                }
                NATIVE -> {
                    val alternatives = calls[nextClause] as NativeAlternatives
                    val argsBase = cps[base + CP_ARGS]
                    val arity = cps[base + CP_ARITY]
                    copyWords(cpArgs, argsBase, regs, 0, arity)
                    popChoicePoint()
                    beginNative(arity)
                    nativeNames = alternatives.names?.let { names -> namesBelowTop(names) }
                    val outcome = guardingNative { alternatives.resume.resume(this) }
                    when (outcome) {
                        FAIL -> Unit
                        CONTINUE -> {
                            pc = continuation
                            return true
                        }
                        else -> return true
                    }
                }
                STORED -> {
                    val call = calls[nextClause] as StoredCall
                    val argsBase = cps[base + CP_ARGS]
                    val arity = cps[base + CP_ARITY]
                    val from = cps[base + CP_KEY]
                    copyWords(cpArgs, argsBase, regs, 0, arity)
                    popChoicePoint()
                    if (stores.tryStored(call, from, arity, nextClause) != FAIL) return true
                }
                else -> if (retryTop(base, functor)) return true
            }
        }
        return false
    }

    // [backtrack] as far as the choice points of clauses and conditions go: false for [backtrack] to go on
    private fun backtrackHere(cpBase: Int): Boolean {
        if (delayedNext >= 0 && retryDelayed()) return true
        while (cpTop > cpBase) {
            val base = (cpTop - 1) * CP_SIZE
            val functor = cps[base + CP_KIND]
            if (functor == RESUME) {
                restoreTop()
                popChoicePoint()
                return true
            }
            if (functor < 0) return false
            restoreTop()
            if (retryTop(base, functor)) return true
        }
        return false
    }

    // The next clause of the choice point on top, at [base], with the clauses of [functor], the machine restored to it
    @Suppress("NOTHING_TO_INLINE")
    private inline fun retryTop(
        base: Int,
        functor: Int,
    ): Boolean {
        val arity = cps[base + CP_ARITY]
        copyWords(cpArgs, cps[base + CP_ARGS], regs, 0, arity)
        val cp = cpTop - 1
        if (cpTables[cp] == null) selectTheoryClauses(functor, arity, cp, base)
        return retryClauses(
            cpTables[cp]!!,
            cpCandidates[cp],
            cps[base + CP_NEXT],
            arity,
            cps[base + CP_BARRIER],
            cps[base + CP_KEY],
            cps[base + CP_SOURCE] == MERGED_FREE,
            base,
        )
    }

    // ---- calls

    private fun keyOfHeap(w: Int): Int {
        val d = deref(w)
        return when (tag(d)) {
            REF -> 0
            STR -> heap[payload(d)]
            LIST -> Words.LIST_KEY
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
            LIST -> Words.LIST_KEY
            else -> d
        }
    }

    // The predicate of [functor] the resolution sees: the theory of the user's, if visible and it defines it (the
    // theory is a module of its own, whatever the libraries define under the same name), else the libraries'
    private fun predicateOf(functor: Int): Predicate? =
        (if (theoryVisible) program.theory.predicate(functor) else null) ?: scope.predicate(functor)

    // Enters the first clause of [predicate] the goal on the registers may match, leaving a choice point if another
    // may too (by the keys of every argument of the clauses: no trailing No for a clause whose head could not unify, as
    // BurdenOfProofTest counts solutions); false when none does
    private fun tryClauses(
        predicate: Predicate,
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
        predicate: Predicate,
        table: ClauseTable,
        key: Int,
        arity: Int,
        barrier: Int,
    ): Boolean {
        val n = table.size
        val firstKeys = table.firstKeys
        if (n >= Predicate.INDEXED_FROM) {
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
    private fun commits(clause: ClauseCode): Boolean {
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
        table: ClauseTable,
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
        predicate: Predicate,
        key: Int,
        arity: Int,
    ): IntArray? = if (key != 0) narrowed(predicate, predicate.candidates(key), arity) else deepCandidates(predicate, arity)

    // The clauses of a large [predicate] a goal with no first argument may match, if the index of the next bound
    // argument that has one says
    private fun deepCandidates(
        predicate: Predicate,
        arity: Int,
    ): IntArray? {
        if (!predicate.deep) return null
        val end = minOf(arity - 1, Predicate.MAX_DEEP_POSITION)
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
        predicate: Predicate,
        byFirst: IntArray,
        arity: Int,
    ): IntArray {
        if (byFirst.size < Predicate.INDEXED_FROM || !predicate.deep) return byFirst
        var best = byFirst
        val end = minOf(arity - 1, Predicate.MAX_DEEP_POSITION)
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
        table: ClauseTable,
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
        cpCandidates[cp] = if (table.size >= Predicate.INDEXED_FROM) selected(predicate, key, arity) else null
        cps[base + CP_KEY] = key
    }

    // The next clause of the choice point on top (at [base]; [NONE]: of the one delayed) from the position [from] among its
    // clauses or its candidates, the registers holding its arguments: the choice point stays for the one after it, if any,
    // and goes otherwise; false when none is left
    private fun retryClauses(
        table: ClauseTable,
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
                truncateCalls(cps[base + CP_CALLS])
                cps[base + CP_NEXT] = next
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
        table: ClauseTable,
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
        table: ClauseTable,
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
        clause: ClauseCode,
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
     * called as the goal itself cuts to: [FAIL], [CONTINUE] (a builtin done, the code goes on at [pc]) or [JUMPED]
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
        return if (tryClauses(predicate, arity, cpTop)) JUMPED else FAIL
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
            FAIL -> -1
            JUMPED -> pc
            else -> if (next >= 0) next else continuation
        }
    }

    // Every so many calls, or the heap above its threshold: the collection, the deadline and the interruption
    private fun housekeeping(arity: Int) {
        if (top > gcThreshold) collect(arity)
        if (callsSinceCheck >= CHECK_EVERY) {
            callsSinceCheck = 0
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
        val looked = scope.lookedUp(functor) ?: return unknownProcedure(functor, arity)
        val primitive = looked.primitive ?: return unknownProcedure(functor, arity)
        val native = looked.native
        if (native != null) {
            val answer = native.run(this)
            if (answer != StdlibNatives.NOT_NATIVE) return answer
        }
        return callPrimitive(primitive, functor, arity)
    }

    // A procedure that neither the clauses nor the libraries define, as the flag unknown says (ISO): an existence error, a
    // warning and a failure, or a failure (the solvers of arg2p fail, as a theory may leave its hooks undefined)
    private fun unknownProcedure(
        functor: Int,
        arity: Int,
    ): Int {
        val name = program.nameOfFunctorId(functor)
        return when (environment.flags[Unknown] ?: Unknown.defaultValue) {
            Unknown.FAIL -> FAIL
            Unknown.WARNING -> {
                val context = context
                context.warnings.write(MissingPredicate(context, Signature(name, arity)))
                FAIL
            }
            else -> throw Raised(error(Struct.of("existence_error", Atom.of("procedure"), Indicator.of(name, arity)), "$name/$arity"))
        }
    }

    /** Calls the goal [w], a term on the heap, its arguments loaded in the registers */
    fun callTerm(w: Int): Int {
        val g = deref(w)
        val functor: Int
        val arity: Int
        // (the compound of a number no word holds is a number, see Numbers)
        when (typeTag(g)) {
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
                        if ((code[at + 3] ushr typeTag(dereferenced(regs[code[at + 4]]))) and 1 == 0) break@dispatch
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
                            if (answer == FAIL) break@dispatch
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
                        regs[code[at + 1]] = ref(newVariable(code[at + 2]))
                        ip = at + 3
                    }
                    Op.SET_Y_VAR -> {
                        local[frame + FRAME + code[at + 1]] = ref(newVariable(code[at + 2]))
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
                            val w = sumWord(payload(a), code[at + 2])
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
                        if ((code[at + 1] ushr typeTag(dereferenced(regs[code[at + 2]]))) and 1 == 0) break@dispatch
                        ip = at + 3
                    }
                    Op.TYPE_Y -> {
                        if ((code[at + 1] ushr typeTag(deref(local[frame + FRAME + code[at + 2]]))) and 1 == 0) break@dispatch
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
                        if ((deref(operand(code[at + 2])) == code[at + 3]) == (code[at + 1] != 0)) break@dispatch
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

    internal fun holds(
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
        if (small != NOT_SMALL) return intWord(small.toLong())
        val value = evaluate(code, from, n)
        return if (evalFailed) 0 else intWord(value)
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
        return if (holds(code[at + 1], a.compareTo(b), 0)) 1 else 0
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
                            STR -> {
                                val long = longOfNumber(w)
                                if (long == null) {
                                    evalFailed = true
                                    return 0
                                }
                                long
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
            FAIL -> -1
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
            FAIL -> -1
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

    // The target of an inline builtin as a word (a first occurrence created where it lives)
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

    // arg/3 with the index [i]: [CONTINUE] or [FAIL], [NOT_INLINE] for what is left to the builtin (what is not compound)
    private fun inlineArg(
        i: Int,
        term0: Int,
        kind: Int,
        operand: Int,
    ): Int {
        val term = dereferenced(term0)
        val w =
            when (typeTag(term)) {
                STR -> {
                    val a = payload(term)
                    if (i < 1 || i > program.arity(heap[a])) return FAIL
                    heap[a + i]
                }
                LIST -> {
                    if (i < 1 || i > 2) return FAIL
                    heap[payload(term) + i - 1]
                }
                else -> return NOT_INLINE
            }
        return if (toTarget(kind, operand, w)) CONTINUE else FAIL
    }

    // functor/3 of a term that is bound: [CONTINUE] or [FAIL], [NOT_INLINE] for what is left to the builtin (a term to build, an arity that is not one)
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
        when (if (t == STR) typeTag(term) else t) {
            STR -> {
                val f = heap[payload(term)]
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

    /** arg/3 with the index [i], its arguments in the registers: [CONTINUE] or [FAIL], negative for what is left to the library */
    fun argOnRegisters(i: Int): Int = inlineArg(i, regs[1], Op.T_X, 2)

    /** functor/3 with its arguments in the registers: [CONTINUE] or [FAIL], negative for what is left to the library */
    fun functorOnRegisters(): Int = inlineFunctor(regs[0], Op.T_X, 1, Op.T_X, 2)

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
    internal fun error(
        type: Term,
        where: String,
    ): Term = Struct.of("error", type, Atom.of(where))

    // catch(Goal, Catcher, Recovery): a choice point the unwinding of a ball stops at, then the goal followed by
    // '$catch_exit'(N), which ends the catch when the goal exits
    internal fun catch(): Int {
        val number = ++catches
        pushChoicePoint(CATCH, 0, 3, 0, continuation, number)
        cutBarrier = cpTop
        val goal = deref(regs[0])
        // (an unbound goal is an instantiation error, raised by callTerm, with the catch active)
        if (tag(goal) == REF) return callTerm(goal)
        val numberWord = intWord(number.toLong())
        val h = top
        if (h + 5 > heap.size) grow(h + 5)
        top = h + 5
        heap[h] = wCatchExit
        heap[h + 1] = numberWord
        heap[h + 2] = wComma
        heap[h + 3] = goal
        heap[h + 4] = word(STR, h)
        return callTerm(word(STR, h + 2))
    }

    // The goal of the catch numbered regs[0] exited: if it left no choice point the catch goes; otherwise it stays,
    // inactive, under a CATCH_EXIT choice point that makes it active again when backtracking goes back into the goal
    internal fun catchExit(): Int {
        settle()
        val number = intValue(deref(regs[0])).toInt()
        var cp = cpTop - 1
        while (cp >= 0 && !(cps[cp * CP_SIZE + CP_KIND] == CATCH && cps[cp * CP_SIZE + CP_CATCH_NUMBER] == number)) cp--
        if (cp < 0) return CONTINUE
        if (cp == cpTop - 1) {
            popChoicePoint()
        } else {
            cps[cp * CP_SIZE + CP_CATCH_INACTIVE] = 1
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
            if (cps[base + CP_KIND] != CATCH || cps[base + CP_CATCH_INACTIVE] != 0) {
                popChoicePoint()
                continue
            }
            restoreTop()
            val argsBase = cps[base + CP_ARGS]
            val catcher = cpArgs[argsBase + 1]
            val recovery = cpArgs[argsBase + 2]
            popChoicePoint()
            val ball = fromTerm(raised.ball, HashMap())
            if (unify(catcher, ball)) {
                cutBarrier = cpTop
                when (callTerm(recovery)) {
                    FAIL -> if (!backtrack(cpBase)) return EXHAUSTED
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
        val context: ExecutionContext = PrologExecutionContext(environment)
        val ball = raised.ball
        return if (ball is Struct && ball.functor == "error" && ball.arity in 1..2 && ball[0] is Struct) {
            try {
                // (the message: the error, and a text as its context, its explanation, whatever tuProlog makes of them)
                val type = ball[0] as Struct
                val extra = if (ball.arity > 1) ball[1] else null
                val message = (extra as? Atom)?.let { "$type: ${it.value}" }
                LogicError.of(message = message, context = context, type = type, extraData = extra)
            } catch (_: IllegalArgumentException) {
                MessageError.of(ball, context)
            }
        } else {
            MessageError.of(ball, context)
        }
    }

    // The ball of a resolution exception, as catch/3 sees it
    fun ballOf(exception: ResolutionException): Term =
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
            STR -> {
                longOfNumber(w)?.let { return it }
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

    internal fun findall(
        template: Int,
        goal: Int,
        result: Int,
    ): Int {
        val pinnedResult = pinWord(result)
        solutions(template, goal, Int.MAX_VALUE)
        val resultWord = pinnedWords[pinnedResult]
        pinnedWordsTop = pinnedResult
        return if (unify(resultWord, collectedList)) CONTINUE else FAIL
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
                        FAIL -> backtrack(cpBase) && run(cpBase)
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
            heap[cell] = shifted(r, shift)
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
        nestedForNative(template, goal, Int.MAX_VALUE)
        return collectedList
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

    /** A copy of the term [w0] on the heap, its unbound variables fresh and unnamed */
    fun copyOnHeap(w0: Int): Int {
        val bagBase = copier.top
        val root = copier.copy(w0)
        val shift = copier.toHeap(bagBase, named = false)
        copier.top = bagBase
        return shifted(root, shift)
    }

    override fun sorted(items: IntArray): IntArray? = termOrder.sorted(items)

    override fun sortedDistinct(items: IntArray): IntArray? = termOrder.sortedDistinct(items)

    override val recordProgram: Program get() = program

    override fun int(value: Int): Int = intWord(value.toLong())

    // The key at hand, of no use past the lookup
    private val keys = TermFlattener(program, 256)

    override fun recordKey(w: Int): RecordKey = keys.keyOf(heap, w)

    override fun record(w: Int): Record {
        val bagBase = copier.top
        val root = copier.copy(w)
        val words = copier.relativeTo(bagBase)
        copier.top = bagBase
        return Record(program, words, shifted(root, -bagBase), null)
    }

    override fun recall(record: Record): Int {
        if (!program.readsWordsOf(record.program)) return fromTerm(record.term, HashMap())
        val words = record.words
        val n = words.size
        val base = alloc(n)
        for (i in 0 until n) {
            val x = words[i]
            heap[base + i] = shifted(x, base)
            varInfo[base + i] = 0
        }
        val r = record.root
        if (record.functor != 0) heap[base + payload(r)] = record.functor
        return shifted(r, base)
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

    // Calls [primitive] on the registers: on its words if native, otherwise through the facade
    internal fun callPrimitive(
        primitive: Primitive,
        functor: Int,
        arity: Int,
    ): Int = if (primitive is NativePrimitive) callNative(primitive, arity) else primitives.call(primitive, functor, arity)

    // ---- native primitives (the machine as their NativeCall)

    private fun callNative(
        primitive: NativePrimitive,
        arity: Int,
    ): Int {
        beginNative(arity)
        return guardingNative { primitive.call(this) }
    }

    private fun beginNative(arity: Int) {
        nativeArity = arity
        nativeNames = null
        nativeFresh = null
        nativeMerged = 0
    }

    // What the code of a native primitive does: any exception of it a ball
    private inline fun guardingNative(call: () -> Int): Int =
        try {
            call()
        } catch (raised: Raised) {
            throw raised
        } catch (halt: Halt) {
            throw halt
        } catch (exception: Exception) {
            throw raised(exception)
        }

    override val arity: Int get() = nativeArity

    override fun arg(i: Int): Int = deref(regs[i])

    override fun kind(w: Int): Int =
        when (tag(w)) {
            REF -> NativeCall.VAR
            ATOM -> NativeCall.ATOM
            INT -> NativeCall.INT
            STR ->
                when (program.numberTag(heap[payload(w)])) {
                    BIG -> NativeCall.INT
                    FLT -> NativeCall.FLOAT
                    else -> NativeCall.STRUCT
                }
            else -> NativeCall.LIST
        }

    override fun atomName(w: Int): String = program.symbols.atoms[payload(w)]

    override fun intValue(w: Int): Long = if (tag(w) == INT) payload(w).toLong() else bigIntegerOfNumber(w).toLong()

    override fun functorName(w: Int): String = if (tag(w) == LIST) "." else program.nameOfFunctorWord(heap[payload(w)])

    override fun arityOf(w: Int): Int = if (tag(w) == LIST) 2 else program.arity(heap[payload(w)])

    override fun argOf(
        w: Int,
        i: Int,
    ): Int = deref(heap[payload(w) + (if (tag(w) == LIST) i else i + 1)])

    override fun atom(name: String): Int = program.atomWord(name)

    override fun int(value: Long): Int = intWord(value)

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
    ): Boolean = HeapTerms.identical(this, a, b)

    override fun hashOf(w: Int): Int = HeapTerms.hash(this, w, 17)

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

    override val libraries: Runtime get() = scope.libraries ?: environment.libraries

    override fun libraryValue(compute: (Runtime) -> Any?): Any? {
        val scope = scope
        val libraries = scope.libraries ?: return compute(environment.libraries)
        if (scope.libraryValueOf !== compute) {
            scope.libraryValue = compute(libraries)
            scope.libraryValueOf = compute
        }
        return scope.libraryValue
    }

    override val theoryVersion: Long? get() = program.theoryVersion

    override val rootLibraries: Runtime get() = environment.libraries

    override fun storeGoal(
        goal: Int,
        store: ClauseStore,
        wrap: GoalWrapper?,
    ): Int = stores.storeGoal(goal, store, wrap)

    override fun storeClause(
        head: Int,
        body: Int,
        store: ClauseStore,
    ): Int = stores.storeClause(head, body, store)

    override fun storeHolds(
        goal: Int,
        body: Int?,
        store: ClauseStore,
    ): Boolean? = stores.storeHolds(goal, body, store)

    override fun storeMatching(
        goal: Int,
        store: ClauseStore,
    ): List<ClauseStore.Stored> = stores.storeMatching(goal, store)

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
        beginNative(0)
        return guardingNative { primitive.callOnRegisters(this, module) }
    }

    private var moduleGoalFunctor = 0
    private var moduleGoalArity = 0

    private fun goalOnRegisters(): Int {
        val arity = moduleGoalArity
        val functor = moduleGoalFunctor
        if (arity == 0) return word(ATOM, program.symbols.functorNames[functor])
        val h = alloc(arity + 1)
        heap[h] = word(FUN, functor)
        for (i in 0 until arity) heap[h + 1 + i] = regs[i]
        return word(STR, h)
    }

    override fun inlineOnRegisters(
        libraries: Runtime,
        theoryVisible: Boolean,
    ): Int {
        enterScope(libraries, theoryVisible)
        return enteredIn(call(moduleGoalFunctor, moduleGoalArity))
    }

    override fun alternatives(resume: NativeResume) {
        pushChoicePoint(NATIVE, calls.size, nativeArity, 0, continuation)
        calls.add(NativeAlternatives(resume, nativeNames))
    }

    // The ball of an exception the code of a primitive raised: a resolution exception as it is, any other a
    // system_error catch/3 can handle
    fun raised(exception: Exception): Raised {
        val resolution =
            exception as? ResolutionException
                ?: SystemError.forUncaughtException(PrologExecutionContext(environment), exception)
        return Raised(ballOf(resolution), resolution)
    }

    // ---- module calls

    // The goal [goal] entered against [libraries] (the theory of the user visible or not), the scope of the caller
    // restored by the frame allocated here when the goal returns
    override fun inline(
        goal: Int,
        libraries: Runtime,
        theoryVisible: Boolean,
    ): Int {
        enterScope(libraries, theoryVisible)
        return enteredIn(callTerm(goal))
    }

    private fun enterScope(
        libraries: Runtime,
        theory: Boolean,
    ) {
        allocate(2)
        local[frame + FRAME] = smallInt(scope.id)
        local[frame + FRAME + 1] = smallInt(if (theoryVisible) 1 else 0)
        continuation = restoreScopeBlock
        scope = program.scopeOf(libraries)
        fitRegisters()
        theoryVisible = theory
        cutBarrier = cpTop
    }

    // What a goal entered in another scope answers, by what its call did: done at once, it goes on at the block restoring the scope
    private fun enteredIn(outcome: Int): Int =
        when (outcome) {
            FAIL -> FAIL
            CONTINUE -> jump(continuation)
            else -> JUMPED
        }

    /** Collects the heap cells no longer reachable, [arity] registers holding the arguments of the call at hand */
    fun collect(arity: Int) = collector.collect(arity)

    /** How many times the heap was collected */
    val collections: Int get() = collector.collections

    // The variables a native call gave the cells below the top: what the top was cut back from is gone
    private fun namesBelowTop(names: HashMap<Int, Var>): HashMap<Int, Var> {
        val below = HashMap<Int, Var>(names.size * 2)
        for ((cell, variable) in names) if (cell < top) below[cell] = variable
        return below
    }

    // ---- terms in and out

    // The terms written on the heap, and read from it
    private val writer =
        object : TermWriter(program) {
            override fun alloc(cells: Int): Int = this@Machine.alloc(cells)

            override fun put(
                address: Int,
                w: Int,
            ) {
                heap[address] = w
            }

            override fun variable(term: Var): Int {
                varObjects.add(term)
                return newVariable(-varObjects.size)
            }
        }

    private val reader =
        object : TermReader(program) {
            override fun cell(address: Int): Int = heap[address]

            // The variable a cell holding an unbound one is read as (see varInfo)
            override fun variable(address: Int): Var {
                val info = varInfo[address]
                return when {
                    info > 0 -> Var.of(program.symbols.atoms[info - 1])
                    info < 0 -> varObjects[-info - 1]
                    else -> Var.of("_G$address")
                }
            }
        }

    /** [term] on the heap, its variables as cells (those of [variables], allocated as met) */
    fun fromTerm(
        term: Term,
        variables: HashMap<Var, Int>,
    ): Int = writer.write(term, variables)

    /** The term of heap word [w0], its unbound cells as the variables of [names] */
    fun toTerm(
        w0: Int,
        names: HashMap<Int, Var>,
    ): Term = reader.read(w0, names)

    /** The machine emptied: a top-level query starts from nothing */
    private fun reset() {
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
        private const val MERGED = 0
        private const val THEORY = 1

        // (as [MERGED], the arguments of the goal beyond the first all unbound)
        private const val MERGED_FREE = 2

        // An inline builtin leaving the call to the builtin itself
        private const val NOT_INLINE = StdlibNatives.NOT_NATIVE

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

// The alternatives of a native call, with the variables its terms gave the cells (the same on resumption, for
// the cells still there)
internal class NativeAlternatives(
    val resume: NativeResume,
    val names: HashMap<Int, Var>?,
)
