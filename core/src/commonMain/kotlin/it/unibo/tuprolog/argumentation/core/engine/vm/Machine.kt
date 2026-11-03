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
 * The machine (spike): a heap of words (see [Words]), a trail of the cells bound since the last choice point, the
 * choice points as records of ints, the continuation (the goals left to run) as nodes of ints linked newest first,
 * and the argument registers of the goal being called. Nothing is allocated per step but cells of these arrays: a
 * clause is entered by allocating the cells of its variables and unifying its head, as stored in the code, against
 * the registers; its body goals become continuation nodes referring to the code and the cells. Backtracking resets
 * the tops of the arrays and undoes the trail.
 *
 * The heap and the continuation nodes are garbage collected ([collect]): the nodes reachable from the goal to run
 * next and from the choice points are kept, in their order, then the cells reachable from them, from the choice
 * points, from the trail and from the words pinned by the code around a nested run, and the rest slides down.
 *
 * The primitives of the libraries ([facade]) are called with their goal as tuProlog terms and their answers unified
 * back; a primitive with further answers leaves a choice point of its own.
 */
internal class Machine(
    val program: Program,
    // The heap cells above which a step collects (the threshold then follows the cells kept)
    private val gcMinimum: Int = GC_MIN,
) {
    var heap = IntArray(1 shl 16)
    var top = 0

    // The cells below this address are trailed when bound (the heap top at the latest choice point)
    private var heapBarrier = 0
    private var trail = IntArray(1 shl 12)
    private var trailTop = 0

    // The continuation nodes: goal word, env (the base of the cells of the clause, or HEAP_GOAL for a goal on the
    // heap), the cut barrier (the choice points to keep on a cut), the next node, the number of cells of the clause
    private var cont = IntArray(NODE shl 12)
    private var contTop = 0

    /** The node of the goal to run next, NIL when none */
    var current = NIL

    private var cps = IntArray(CP_SIZE shl 10)
    private var cpTop = 0
    private var cpArgs = IntArray(1 shl 12)
    private var cpArgsTop = 0

    /** The arguments of the goal being called */
    var regs = IntArray(64)

    private var pdl = IntArray(1 shl 10)

    // The words and nodes the code around a nested run holds: relocated by the collector
    private var pinnedWords = IntArray(16)
    private var pinnedWordsTop = 0
    private var pinnedNodes = IntArray(16)
    private var pinnedNodesTop = 0

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
    private var nodeForward = IntArray(0)

    /** How many times the heap was collected, and how many cells it held before and after the last collection */
    var collections = 0
        private set
    var collectedFrom = 0
        private set
    var collectedTo = 0
        private set

    fun interface Builtin {
        /** Runs the builtin on the registers of the machine; false on failure */
        fun call(
            machine: Machine,
            cutBarrier: Int,
        ): Boolean
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

    // Control functors and goals
    private val fIfThen = program.functorWord("->", 2)
    private val fCutTo = program.functor("\$cut", 1)
    private val fNaf = program.functor("\$naf", 1)
    private val failGoal = program.atomWord("fail")
    private val stopGoal = program.atomWord("\$stop")

    init {
        builtin("true", 0) { _, _ -> true }
        builtin("fail", 0) { _, _ -> false }
        builtin("false", 0) { _, _ -> false }
        builtin("!", 0) { m, cut ->
            m.cutTo(cut)
            true
        }
        builtin("\$cut", 1) { m, _ ->
            m.cutTo(payload(m.deref(m.regs[0])))
            true
        }
        builtin("\$naf", 1) { m, _ ->
            m.cutTo(payload(m.deref(m.regs[0])))
            false
        }
        builtin("=", 2) { m, _ -> m.unify(m.regs[0], m.regs[1]) }
        builtin("is", 2) { m, _ -> m.unify(m.regs[0], m.program.intWord(m.eval(m.regs[1]))) }
        builtin("<", 2) { m, _ -> m.eval(m.regs[0]) < m.eval(m.regs[1]) }
        builtin(">", 2) { m, _ -> m.eval(m.regs[0]) > m.eval(m.regs[1]) }
        builtin("=<", 2) { m, _ -> m.eval(m.regs[0]) <= m.eval(m.regs[1]) }
        builtin(">=", 2) { m, _ -> m.eval(m.regs[0]) >= m.eval(m.regs[1]) }
        builtin("=:=", 2) { m, _ -> m.eval(m.regs[0]) == m.eval(m.regs[1]) }
        builtin("=\\=", 2) { m, _ -> m.eval(m.regs[0]) != m.eval(m.regs[1]) }
        builtin("var", 1) { m, _ -> tag(m.deref(m.regs[0])) == REF }
        builtin("nonvar", 1) { m, _ -> tag(m.deref(m.regs[0])) != REF }
        builtin("call", 1) { m, _ ->
            m.current = m.pushNode(m.regs[0], HEAP_GOAL, m.cpTop, m.current)
            true
        }
        builtin(",", 2) { m, cut ->
            // (transparent to cut: the goals keep the barrier of the clause)
            m.current = m.pushNode(m.regs[0], HEAP_GOAL, cut, m.pushNode(m.regs[1], HEAP_GOAL, cut, m.current))
            true
        }
        builtin(";", 2) { m, cut ->
            val left = m.deref(m.regs[0])
            if (tag(left) == STR && m.heap[payload(left)] == fIfThen) {
                val a = payload(left)
                m.ifThenElse(m.heap[a + 1], m.heap[a + 2], m.regs[1], cut)
            } else {
                val right = m.pushNode(m.regs[1], HEAP_GOAL, cut, m.current)
                m.resumeChoicePoint(right)
                m.current = m.pushNode(m.regs[0], HEAP_GOAL, cut, m.current)
            }
            true
        }
        builtin("->", 2) { m, cut ->
            m.ifThenElse(m.regs[0], m.regs[1], failGoal, cut)
            true
        }
        builtin("\\+", 1) { m, _ ->
            val barrier = m.cpTop
            m.resumeChoicePoint(m.current)
            val naf = m.alloc(2)
            m.heap[naf] = word(FUN, fNaf)
            m.heap[naf + 1] = Words.int(barrier)
            m.current = m.pushNode(m.regs[0], HEAP_GOAL, m.cpTop, m.pushNode(word(STR, naf), HEAP_GOAL, 0, m.current))
            true
        }
        builtin("findall", 3) { m, _ -> m.findall(m.regs[0], m.regs[1], m.regs[2]) }
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
    }

    /** The primitives of the libraries of [environment] answer the goals the program and the builtins do not */
    fun facade(environment: ExecutionEnvironment) {
        this.environment = environment
        primitiveTable = arrayOfNulls(0)
        primitiveLooked = BooleanArray(0)
        // (the clauses of the libraries, as any program clause: the builtins still answer the goals they do)
        for (library in environment.libraries.libraries) program.add(library.clauses)
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

    // ---- code to heap

    /** The term of code word [w] on the heap, its variable slots as the cells from [env] */
    fun fromCode(
        w: Int,
        env: Int,
    ): Int {
        val code = program.code
        return when (tag(w)) {
            REF -> ref(env + payload(w))
            STR -> {
                val c = payload(w)
                val f = code[c]
                val n = program.arity(f)
                val s = alloc(n + 1)
                heap[s] = f
                for (i in 1..n) {
                    val arg = fromCode(code[c + i], env)
                    heap[s + i] = arg
                }
                word(STR, s)
            }
            LIST -> {
                val first = alloc(2)
                var s = first
                var c = payload(w)
                while (true) {
                    val head = fromCode(code[c], env)
                    heap[s] = head
                    val t = code[c + 1]
                    if (tag(t) == LIST) {
                        val s2 = alloc(2)
                        heap[s + 1] = word(LIST, s2)
                        s = s2
                        c = payload(t)
                    } else {
                        val tail = fromCode(t, env)
                        heap[s + 1] = tail
                        break
                    }
                }
                word(LIST, first)
            }
            else -> w
        }
    }

    /** Unifies the code word [w] (a term of a clause head, its slots from [env]) with the heap word [h0] */
    private fun unifyCode(
        w: Int,
        env: Int,
        h0: Int,
    ): Boolean {
        val code = program.code
        when (tag(w)) {
            REF -> return unify(ref(env + payload(w)), h0)
            STR -> {
                val h = deref(h0)
                if (tag(h) == REF) {
                    bind(payload(h), fromCode(w, env))
                    return true
                }
                if (tag(h) != STR) return false
                val c = payload(w)
                val a = payload(h)
                if (code[c] != heap[a]) return false
                val n = program.arity(code[c])
                for (i in 1..n) if (!unifyCode(code[c + i], env, heap[a + i])) return false
                return true
            }
            LIST -> {
                var cw = w
                var hh = h0
                while (true) {
                    val h = deref(hh)
                    if (tag(h) == REF) {
                        bind(payload(h), fromCode(cw, env))
                        return true
                    }
                    if (tag(h) != LIST) return false
                    val c = payload(cw)
                    val a = payload(h)
                    if (!unifyCode(code[c], env, heap[a])) return false
                    val ct = code[c + 1]
                    if (tag(ct) == LIST) {
                        cw = ct
                        hh = heap[a + 1]
                    } else {
                        return unifyCode(ct, env, heap[a + 1])
                    }
                }
            }
            else -> {
                val h = deref(h0)
                if (tag(h) == REF) {
                    bind(payload(h), w)
                    return true
                }
                return h == w
            }
        }
    }

    // ---- continuation and choice points

    fun pushNode(
        goal: Int,
        env: Int,
        cutBarrier: Int,
        next: Int,
        envSize: Int = 0,
    ): Int {
        if (contTop + NODE > cont.size) cont = cont.copyOf(cont.size * 2)
        val node = contTop
        cont[node] = goal
        cont[node + 1] = env
        cont[node + 2] = cutBarrier
        cont[node + 3] = next
        cont[node + 4] = envSize
        contTop += NODE
        return node
    }

    private fun pushChoicePoint(
        functor: Int,
        nextClause: Int,
        arity: Int,
        cutBarrier: Int,
    ) {
        val base = cpTop * CP_SIZE
        if (base + CP_SIZE > cps.size) cps = cps.copyOf(cps.size * 2)
        cps[base] = trailTop
        cps[base + 1] = top
        cps[base + 2] = contTop
        cps[base + 3] = current
        cps[base + 4] = functor
        cps[base + 5] = nextClause
        cps[base + 6] = cutBarrier
        cps[base + 7] = cpArgsTop
        cps[base + 8] = primitiveCalls.size
        if (cpArgsTop + arity > cpArgs.size) cpArgs = cpArgs.copyOf(maxOf(cpArgs.size * 2, cpArgsTop + arity))
        regs.copyInto(cpArgs, cpArgsTop, 0, arity)
        cpArgsTop += arity
        cpTop++
        heapBarrier = top
    }

    // A choice point that, backtracked into, goes on from [node]
    private fun resumeChoicePoint(node: Int) {
        val saved = current
        current = node
        pushChoicePoint(RESUME, 0, 0, 0)
        current = saved
    }

    // Restores the state saved by the top choice point (which stays)
    private fun restoreTop() {
        val base = (cpTop - 1) * CP_SIZE
        undoTrail(cps[base])
        top = cps[base + 1]
        contTop = cps[base + 2]
        current = cps[base + 3]
    }

    private fun popChoicePoint() {
        cpTop--
        val base = cpTop * CP_SIZE
        cpArgsTop = cps[base + 7]
        truncatePrimitiveCalls(cps[base + 8])
        heapBarrier = if (cpTop > 0) cps[(cpTop - 1) * CP_SIZE + 1] else 0
    }

    private fun truncatePrimitiveCalls(size: Int) {
        while (primitiveCalls.size > size) primitiveCalls.removeAt(primitiveCalls.size - 1)
    }

    fun cutTo(cutBarrier: Int) {
        if (cpTop > cutBarrier) {
            val base = cutBarrier * CP_SIZE
            cpArgsTop = cps[base + 7]
            truncatePrimitiveCalls(cps[base + 8])
            cpTop = cutBarrier
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
            val functor = cps[base + 4]
            val next = cps[base + 5]
            val cutBarrier = cps[base + 6]
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
                    val call = primitiveCalls[next]
                    val response = call.responses.next()
                    if (!call.responses.hasNext()) popChoicePoint()
                    if (applyResponse(response, call)) return true
                }
                else -> {
                    val argsBase = cps[base + 7]
                    val arity = program.functorArities[functor]
                    cpArgs.copyInto(regs, 0, argsBase, argsBase + arity)
                    popChoicePoint()
                    if (tryClauses(program.predicateTable[functor]!!, next, arity, cutBarrier)) return true
                }
            }
        }
        return false
    }

    // ---- resolution

    /** Runs the goal at [current]: false on failure */
    fun step(): Boolean {
        if (top > gcThreshold) collect()
        val node = current
        val goal = cont[node]
        val env = cont[node + 1]
        val cutBarrier = cont[node + 2]
        current = cont[node + 3]
        val functor: Int
        val arity: Int
        if (env != HEAP_GOAL && tag(goal) != REF) {
            // a goal of a clause body, as code
            if (tag(goal) == ATOM) {
                functor = program.functor(payload(goal), 0)
                arity = 0
            } else {
                val code = program.code
                val c = payload(goal)
                val f = code[c]
                functor = payload(f)
                arity = program.arity(f)
                if (arity > regs.size) regs = regs.copyOf(arity * 2)
                for (i in 0 until arity) regs[i] = fromCode(code[c + 1 + i], env)
            }
        } else {
            val g = deref(if (env == HEAP_GOAL) goal else ref(env + payload(goal)))
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
                    functor = program.functor(".", 2)
                    arity = 2
                    regs[0] = heap[a]
                    regs[1] = heap[a + 1]
                }
                REF -> throw IllegalStateException("instantiation_error: the goal is a variable")
                else -> throw IllegalStateException("type_error(callable)")
            }
        }
        if (functor < builtinTable.size) builtinTable[functor]?.let { return it.call(this, cutBarrier) }
        program.predicateTable[functor]?.let { return tryClauses(it, 0, arity, cpTop) }
        val primitive = primitiveFor(functor) ?: return false
        return callPrimitive(primitive, functor, arity)
    }

    private fun keyOfHeap(w: Int): Int {
        val d = deref(w)
        return when (tag(d)) {
            REF -> 0
            STR -> heap[payload(d)]
            LIST -> Program.LIST_KEY
            else -> d
        }
    }

    private fun tryClauses(
        predicate: Program.Predicate,
        from: Int,
        arity: Int,
        cutBarrier: Int,
    ): Boolean {
        val clauses = predicate.clauses
        val size = clauses.size
        val key = if (arity == 0) 0 else keyOfHeap(regs[0])
        var i = from
        while (i < size && !matches(clauses[i].key, key)) i++
        if (i == size) return false
        var j = i + 1
        while (j < size && !matches(clauses[j].key, key)) j++
        if (j < size) pushChoicePoint(predicate.functor, j, arity, cutBarrier)
        return enter(clauses[i], arity, cutBarrier)
    }

    private fun matches(
        clauseKey: Int,
        goalKey: Int,
    ): Boolean = clauseKey == 0 || goalKey == 0 || clauseKey == goalKey

    private fun enter(
        clause: Program.ClauseCode,
        arity: Int,
        cutBarrier: Int,
    ): Boolean {
        val variables = clause.variables
        val env = alloc(variables)
        for (i in 0 until variables) heap[env + i] = ref(env + i)
        val headArgs = clause.headArgs
        for (i in 0 until arity) if (!unifyCode(headArgs[i], env, regs[i])) return false
        val body = clause.body
        var next = current
        for (k in body.indices.reversed()) next = pushNode(body[k], env, cutBarrier, next, variables)
        current = next
        return true
    }

    /**
     * Runs until the continuation reaches the node pinned at [stopPin] (true: a solution; the end of the
     * continuation when [stopPin] is negative) or no choice point above [cpBase] is left
     */
    fun run(
        stopPin: Int,
        cpBase: Int,
    ): Boolean {
        while (true) {
            if (current == (if (stopPin < 0) NIL else pinnedNodes[stopPin])) return true
            if (!step() && !backtrack(cpBase)) return false
        }
    }

    // ---- control

    private fun ifThenElse(
        condition: Int,
        then: Int,
        otherwise: Int,
        cutBarrier: Int,
    ) {
        val barrier = cpTop
        resumeChoicePoint(pushNode(otherwise, HEAP_GOAL, cutBarrier, current))
        val cut = alloc(2)
        heap[cut] = word(FUN, fCutTo)
        heap[cut + 1] = Words.int(barrier)
        current =
            pushNode(
                condition,
                HEAP_GOAL,
                cpTop,
                pushNode(word(STR, cut), HEAP_GOAL, 0, pushNode(then, HEAP_GOAL, cutBarrier, current)),
            )
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

    private fun pinNode(node: Int): Int {
        if (pinnedNodesTop == pinnedNodes.size) pinnedNodes = pinnedNodes.copyOf(pinnedNodes.size * 2)
        pinnedNodes[pinnedNodesTop] = node
        return pinnedNodesTop++
    }

    // ---- findall

    private fun findall(
        template: Int,
        goal: Int,
        result: Int,
    ): Boolean {
        val pinnedWordsBase = pinnedWordsTop
        val pinnedNodesBase = pinnedNodesTop
        val pinnedTemplate = pinWord(template)
        val pinnedResult = pinWord(result)
        val pinnedCurrent = pinNode(current)
        pushChoicePoint(BARRIER, 0, 0, cpTop)
        val cpBase = cpTop
        val stopPin = pinNode(pushNode(stopGoal, HEAP_GOAL, 0, NIL))
        current = pushNode(goal, HEAP_GOAL, cpTop, pinnedNodes[stopPin])
        var bag = IntArray(64)
        var bagTop = 0
        var roots = IntArray(16)
        var count = 0
        var found = run(stopPin, cpBase)
        while (found) {
            if (count == roots.size) roots = roots.copyOf(count * 2)
            val copier = Copier(bag, bagTop)
            val root = copier.copy(deref(pinnedWords[pinnedTemplate]))
            bag = copier.bag
            bagTop = copier.top
            roots[count++] = root
            found = backtrack(cpBase) && run(stopPin, cpBase)
        }
        // The barrier choice point: the state before the goal
        restoreTop()
        popChoicePoint()
        current = pinnedNodes[pinnedCurrent]
        val resultWord = pinnedWords[pinnedResult]
        pinnedWordsTop = pinnedWordsBase
        pinnedNodesTop = pinnedNodesBase
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
        return unify(resultWord, list)
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
    // choice point
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
            pushChoicePoint(PRIMITIVE, primitiveCalls.size, 0, 0)
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

    /** Collects the continuation nodes and the heap cells no longer reachable */
    fun collect() {
        collections++
        collectedFrom = top
        compactNodes()
        // Marking: the cells reachable from the roots
        if (forward.size < top) forward = IntArray(maxOf(top, heap.size))
        forward.fill(-1, 0, top)
        var node = 0
        while (node < contTop) {
            val env = cont[node + 1]
            if (env == HEAP_GOAL) markWord(cont[node]) else for (c in env until env + cont[node + 4]) markCell(c)
            node += NODE
        }
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
        for (i in 0 until cpArgsTop) cpArgs[i] = relocated(cpArgs[i])
        for (i in 0 until trailTop) trail[i] = forward[trail[i]]
        for (i in 0 until pinnedWordsTop) pinnedWords[i] = relocated(pinnedWords[i])
        for (call in primitiveCalls) for (i in call.cells.indices) call.cells[i] = forward[call.cells[i]]
        node = 0
        while (node < contTop) {
            if (cont[node + 1] == HEAP_GOAL) cont[node] = relocated(cont[node]) else cont[node + 1] = newAddress(cont[node + 1])
            node += NODE
        }
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

    // The nodes reachable from the goal to run next, the choice points and the pinned ones kept, in their order
    private fun compactNodes() {
        val nodes = contTop / NODE
        if (nodeForward.size < nodes) nodeForward = IntArray(maxOf(nodes, cont.size / NODE))
        nodeForward.fill(-1, 0, nodes)
        markNodes(current)
        for (i in 0 until pinnedNodesTop) markNodes(pinnedNodes[i])
        for (cp in 0 until cpTop) markNodes(cps[cp * CP_SIZE + 3])
        var kept = 0
        for (i in 0 until nodes) {
            if (nodeForward[i] != -1) {
                nodeForward[i] = kept
                kept++
            } else {
                nodeForward[i] = -kept - 1
            }
        }
        for (i in 0 until nodes) {
            val f = nodeForward[i]
            if (f < 0) continue
            val from = i * NODE
            val to = f * NODE
            cont[to] = cont[from]
            cont[to + 1] = cont[from + 1]
            cont[to + 2] = cont[from + 2]
            cont[to + 3] = newNode(cont[from + 3])
            cont[to + 4] = cont[from + 4]
        }
        current = newNode(current)
        for (i in 0 until pinnedNodesTop) pinnedNodes[i] = newNode(pinnedNodes[i])
        for (cp in 0 until cpTop) {
            val base = cp * CP_SIZE
            cps[base + 3] = newNode(cps[base + 3])
            val mark = cps[base + 2] / NODE
            cps[base + 2] = NODE * (if (mark >= nodes) kept else nodeForward[mark].let { if (it >= 0) it else -it - 1 })
        }
        contTop = kept * NODE
    }

    private fun markNodes(start: Int) {
        var node = start
        while (node != NIL) {
            val i = node / NODE
            if (nodeForward[i] != -1) return
            nodeForward[i] = 0
            node = cont[node + 3]
        }
    }

    private fun newNode(node: Int): Int = if (node == NIL) NIL else nodeForward[node / NODE] * NODE

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
        contTop = 0
        cpTop = 0
        cpArgsTop = 0
        pinnedWordsTop = 0
        pinnedNodesTop = 0
        primitiveCalls.clear()
        current = NIL
    }

    /** The solutions of [query]: each the bindings of its variables, until none is left */
    fun solve(query: Struct): Sequence<Map<Var, Term>> =
        sequence {
            reset()
            val variables = HashMap<Var, Int>()
            val goal = fromTerm(query, variables)
            // (the cells of the variables of the query, pinned: the collector moves them)
            val pins = HashMap<Var, Int>()
            for ((variable, cell) in variables) pins[variable] = pinWord(ref(cell))
            current = pushNode(goal, HEAP_GOAL, 0, NIL)
            var found = run(NIL, 0)
            while (found) {
                val names = HashMap<Int, Var>()
                // (the variables left unbound are not bindings)
                yield(
                    pins.entries
                        .filter { (_, pin) -> tag(deref(pinnedWords[pin])) != REF }
                        .associate { (variable, pin) -> variable to toTerm(pinnedWords[pin], names) },
                )
                found = backtrack(0) && run(NIL, 0)
            }
        }

    companion object {
        const val NIL = -1
        const val HEAP_GOAL = -1
        const val NODE = 5
        const val CP_SIZE = 9

        // The functors of the choice points that are not clause alternatives
        const val BARRIER = -2
        const val RESUME = -3
        const val PRIMITIVE = -4

        // The heap cells above which a step collects
        const val GC_MIN = 1 shl 20
    }
}
