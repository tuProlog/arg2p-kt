package it.unibo.tuprolog.argumentation.engine.vm

import it.unibo.tuprolog.argumentation.engine.natives.GoalWrapper
import it.unibo.tuprolog.argumentation.engine.natives.MachineCall
import it.unibo.tuprolog.argumentation.engine.natives.NativeCall
import it.unibo.tuprolog.argumentation.engine.natives.NativeResume
import it.unibo.tuprolog.argumentation.engine.natives.words
import it.unibo.tuprolog.argumentation.engine.store.ClauseStore
import it.unibo.tuprolog.argumentation.engine.store.Record
import it.unibo.tuprolog.argumentation.engine.store.RecordKey
import it.unibo.tuprolog.argumentation.engine.vm.Machine.Companion.NATIVE
import it.unibo.tuprolog.argumentation.engine.vm.Machine.Companion.NONE
import it.unibo.tuprolog.argumentation.engine.vm.PrologExecutionContext
import it.unibo.tuprolog.argumentation.engine.vm.program.Program
import it.unibo.tuprolog.argumentation.engine.words.Words.ATOM
import it.unibo.tuprolog.argumentation.engine.words.Words.BIG
import it.unibo.tuprolog.argumentation.engine.words.Words.FLT
import it.unibo.tuprolog.argumentation.engine.words.Words.INT
import it.unibo.tuprolog.argumentation.engine.words.Words.LIST
import it.unibo.tuprolog.argumentation.engine.words.Words.REF
import it.unibo.tuprolog.argumentation.engine.words.Words.STR
import it.unibo.tuprolog.argumentation.engine.words.payload
import it.unibo.tuprolog.argumentation.engine.words.ref
import it.unibo.tuprolog.argumentation.engine.words.shifted
import it.unibo.tuprolog.argumentation.engine.words.tag
import it.unibo.tuprolog.argumentation.engine.words.word
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.library.Runtime
import kotlin.jvm.JvmField

/**
 * The machine [m] as the native primitives see it (see [NativeCall], [MachineCall]): the words of the goal and of the terms
 * they read and build, the registers of the call, the nested runs and the lookups they ask for. The natives of the libraries
 * get this object, not the machine: what they may do is what this says. The machine's own natives (StdlibNatives) and its
 * builtins (MachineBuiltins) take the machine itself, as they read its registers and heap on the hot path.
 */
internal class NativeFace(
    @JvmField val m: Machine,
) : MachineCall {
    // (the heap of the machine, held: one load less on each cell read)
    private val heap = m.heap

    override fun deref(w0: Int): Int = heap.deref(w0)

    override fun unify(
        x0: Int,
        y0: Int,
    ): Boolean = heap.unify(x0, y0)

    override fun findall(
        template: Int,
        goal: Int,
    ): Int {
        m.nested.forNative(template, goal, Int.MAX_VALUE)
        return m.nested.collected
    }

    override fun countSolutions(
        goal: Int,
        limit: Int,
    ): Int = m.nested.forNative(NONE, goal, limit)

    override fun sorted(items: IntArray): IntArray? = m.termOrder.sorted(items)

    override fun sortedDistinct(items: IntArray): IntArray? = m.termOrder.sortedDistinct(items)

    override val recordProgram: Program get() = m.program

    override fun int(value: Int): Int = heap.intWord(value.toLong())

    override fun recordKey(w: Int): RecordKey = m.keys.keyOf(heap.cells, w)

    override fun record(w: Int): Record {
        val bagBase = m.copier.top
        val root = m.copier.copy(w)
        val words = m.copier.relativeTo(bagBase)
        m.copier.top = bagBase
        return Record(m.program, words, shifted(root, -bagBase), null)
    }

    override fun recall(record: Record): Int {
        if (!m.program.readsWordsOf(record.program)) return heap.fromTerm(record.term, HashMap())
        val words = record.words
        val n = words.size
        val base = heap.alloc(n)
        for (i in 0 until n) {
            val x = words[i]
            heap.cells[base + i] = shifted(x, base)
            heap.varInfo[base + i] = 0
        }
        val r = record.root
        if (record.functor != 0) heap.cells[base + payload(r)] = record.functor
        return shifted(r, base)
    }

    override val arity: Int get() = m.natives.arity

    override fun arg(i: Int): Int = deref(m.regs[i])

    override fun kind(w: Int): Int =
        when (tag(w)) {
            REF -> NativeCall.VAR
            ATOM -> NativeCall.ATOM
            INT -> NativeCall.INT
            STR ->
                when (m.program.numberTag(heap.cells[payload(w)])) {
                    BIG -> NativeCall.INT
                    FLT -> NativeCall.FLOAT
                    else -> NativeCall.STRUCT
                }
            else -> NativeCall.LIST
        }

    override fun atomName(w: Int): String = m.program.symbols.atoms[payload(w)]

    // (an integer beyond 64 bits refused, not truncated)
    override fun intValue(w: Int): Long {
        if (tag(w) == INT) return payload(w).toLong()
        val value = heap.bigIntegerOfNumber(w)
        require(value.bitLength < 64) { "An integer beyond 64 bits: $value" }
        return value.toLong()
    }

    override fun functorName(w: Int): String = if (tag(w) == LIST) "." else m.program.nameOfFunctorWord(heap.cells[payload(w)])

    override fun arityOf(w: Int): Int = if (tag(w) == LIST) 2 else m.program.arity(heap.cells[payload(w)])

    override fun argOf(
        w: Int,
        i: Int,
    ): Int = deref(heap.cells[payload(w) + (if (tag(w) == LIST) i else i + 1)])

    override fun atom(name: String): Int = m.program.atomWord(name)

    override fun int(value: Long): Int = heap.intWord(value)

    override fun struct(
        name: String,
        args: IntArray,
    ): Int {
        if (args.isEmpty()) return m.program.atomWord(name)
        val h = heap.alloc(args.size + 1)
        heap.cells[h] = m.program.functorWord(name, args.size)
        for (i in args.indices) heap.cells[h + 1 + i] = args[i]
        return word(STR, h)
    }

    override fun list(
        items: IntArray,
        tail: Int,
    ): Int {
        var list = tail
        for (k in items.indices.reversed()) {
            val cell = heap.alloc(2)
            heap.cells[cell] = items[k]
            heap.cells[cell + 1] = list
            list = word(LIST, cell)
        }
        return list
    }

    override val emptyList: Int get() = m.program.emptyList

    override fun unifiable(
        a: Int,
        b: Int,
    ): Boolean {
        // (every binding trailed, then undone)
        return heap.tentatively { heap.unify(a, b) }
    }

    override fun identical(
        a: Int,
        b: Int,
    ): Boolean = HeapTerms.identical(m, a, b)

    override fun hashOf(w: Int): Int = HeapTerms.hash(m, w, 17)

    override fun variable(): Int = ref(heap.newVariable())

    override fun term(w: Int): Term = heap.toTerm(w, m.natives.namesOfCells())

    override fun word(term: Term): Int = heap.fromTerm(term, m.natives.cellsOfVariables())

    override val context: ExecutionContext get() = PrologExecutionContext(m.environmentSeen())

    override val libraries: Runtime get() = m.scope.libraries ?: m.environment.libraries

    override fun libraryValue(compute: (Runtime) -> Any?): Any? {
        val scope = m.scope
        val libraries = scope.libraries ?: return compute(m.environment.libraries)
        if (scope.libraryValueOf !== compute) {
            scope.libraryValue = compute(libraries)
            scope.libraryValueOf = compute
        }
        return scope.libraryValue
    }

    override val theoryVersion: Long? get() = m.program.theoryVersion

    override val rootLibraries: Runtime get() = m.environment.libraries

    override fun storeGoal(
        goal: Int,
        store: ClauseStore,
        wrap: GoalWrapper?,
    ): Int = m.stores.storeGoal(goal, store, wrap)

    override fun storeClause(
        head: Int,
        body: Int,
        store: ClauseStore,
    ): Int = m.stores.storeClause(head, body, store)

    override fun storeHolds(
        goal: Int,
        body: Int?,
        store: ClauseStore,
    ): Boolean? = m.stores.storeHolds(goal, body, store)

    override fun storeMatching(
        goal: Int,
        store: ClauseStore,
    ): List<ClauseStore.Stored> = m.stores.storeMatching(goal, store)

    override fun inlineOnRegisters(
        libraries: Runtime,
        theoryVisible: Boolean,
    ): Int {
        m.enterScope(libraries, theoryVisible)
        return m.enteredIn(m.call(m.moduleGoalFunctor, m.moduleGoalArity))
    }

    override fun alternatives(resume: NativeResume) {
        m.pushChoicePoint(NATIVE, m.calls.size, m.natives.arity, 0, m.continuation)
        m.calls.add(NativeAlternatives(resume, m.natives.names))
    }

    // The goal [goal] entered against [libraries] (the theory of the user visible or not), the scope of the caller
    // restored by the frame allocated here when the goal returns
    override fun inline(
        goal: Int,
        libraries: Runtime,
        theoryVisible: Boolean,
    ): Int {
        m.enterScope(libraries, theoryVisible)
        return m.enteredIn(m.callTerm(goal))
    }
}
