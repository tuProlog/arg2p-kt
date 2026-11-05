package it.unibo.tuprolog.argumentation.core.engine

import it.unibo.tuprolog.argumentation.core.engine.vm.Program
import it.unibo.tuprolog.argumentation.core.primitives.PrimitiveWithSignature
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.library.Runtime
import it.unibo.tuprolog.solve.primitive.Primitive
import it.unibo.tuprolog.solve.primitive.Solve

/**
 * A primitive native to the machine: [call] runs it on its arguments as words of the machine, read, unified and
 * built through the [NativeCall], without materialising them as terms (the facade does that for any other
 * [Primitive]: the goal built as a term, its answer taken back as a substitution). A [Primitive] still, for the
 * solvers that are not the machine: [AbstractNativePrimitive] gives [solve] for free, over a [NativeCall] backed
 * by terms. A tuProlog exception thrown by [call] is raised as the facade raises it.
 */
interface NativePrimitive : Primitive {
    /** Runs the primitive: [NativeCall.FAIL], [NativeCall.CONTINUE] (done) or [NativeCall.JUMPED] (a goal was entered) */
    fun call(call: NativeCall): Int
}

/**
 * A primitive on a goal the machine holds with its arguments in the registers, no term of it built ([module]: the word of
 * the atom it comes with)
 */
interface RegisterGoalPrimitive {
    fun callOnRegisters(
        call: RegisterGoalCall,
        module: Int,
    ): Int
}

/** What the machine adds to a [NativeCall] for the primitives of arg2p, past what a call on terms can do */
internal interface MachineCall : NativeCall {
    /** What [compute] finds in [libraries], kept for them (one value: the context library) */
    fun libraryValue(compute: (Runtime) -> Any?): Any?

    /** The key [record] of the term [w] would have, with no copy of it, valid for this call; null if it takes the record */
    fun recordKey(w: Int): RecordKey?

    /** The program the words of this call are of */
    val recordProgram: Program
}

/** What a [RegisterGoalPrimitive] finds in the machine */
interface RegisterGoalCall : NativeCall {
    /** The goal as a term on the heap, if it is needed */
    fun goalOnRegisters(): Int

    /** The goal entered against [libraries], as [NativeCall.inline] */
    fun inlineOnRegisters(
        libraries: it.unibo.tuprolog.solve.library.Runtime,
        theoryVisible: Boolean,
    ): Int
}

/**
 * The base of a primitive written once, against the [NativeCall]: the machine runs [call] on its words, any other
 * solver runs it over the terms of the request ([solve], which a primitive with a better term-level resolution
 * may override). The helpers of `NativeCalls.kt` read the arguments, reply and give answers.
 */
abstract class AbstractNativePrimitive(
    override val signature: Signature,
) : NativePrimitive,
    PrimitiveWithSignature {
    override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> = TermCall.solve(this, request)
}

/** The alternatives of a nondeterministic [NativePrimitive] (see [NativeCall.alternatives]) */
fun interface NativeResume {
    /** The next answer, the arguments back in place: [NativeCall.FAIL] when none is left */
    fun resume(call: NativeCall): Int
}

/**
 * The machine as a [NativePrimitive] sees it: the words of its arguments, those it builds, and what it may ask of
 * the resolution. A word read from the heap is valid for the call alone (the heap moves at a collection and is cut
 * back on backtracking): a primitive keeping data between calls keeps terms ([term]), not words.
 */
interface NativeCall {
    /** How many arguments the call has */
    val arity: Int

    /** The argument [i], dereferenced */
    fun arg(i: Int): Int

    fun deref(w: Int): Int

    /** The kind of a dereferenced word: [VAR], [ATOM], [INT], [FLOAT], [STRUCT] or [LIST] (a cell: `[]` is an atom) */
    fun kind(w: Int): Int

    fun isCallable(w: Int): Boolean {
        val k = kind(w)
        return k == ATOM || k == STRUCT || k == LIST
    }

    fun atomName(w: Int): String

    fun intValue(w: Int): Long

    /** The name of a structure (`.` for a list cell) and its arity */
    fun functorName(w: Int): String

    fun arityOf(w: Int): Int

    /** The argument [i] (from 0) of a structure or of a list cell (0 the head, 1 the tail), dereferenced */
    fun argOf(
        w: Int,
        i: Int,
    ): Int

    fun unify(
        a: Int,
        b: Int,
    ): Boolean

    /** Whether [a] and [b] unify, neither bound */
    fun unifiable(
        a: Int,
        b: Int,
    ): Boolean

    /** Whether [a] and [b] are identical, as `==` */
    fun identical(
        a: Int,
        b: Int,
    ): Boolean

    /** A hash of [w], the same for the terms [identical] to it, valid for the call alone */
    fun hashOf(w: Int): Int

    fun atom(name: String): Int

    fun int(value: Long): Int

    /** The word of the integer [value], with no Long where the solver can (a BigInt on JS) */
    fun int(value: Int): Int = int(value.toLong())

    fun struct(
        name: String,
        args: IntArray,
    ): Int

    fun list(
        items: IntArray,
        tail: Int = emptyList,
    ): Int

    val emptyList: Int

    /** A fresh variable */
    fun variable(): Int

    /** The word as a term (its unbound variables as fresh [it.unibo.tuprolog.core.Var]s, the same within one call) */
    fun term(w: Int): Term

    /** The term as a word on the heap */
    fun word(term: Term): Int

    /** The context of the call, for errors and for what the libraries keep (its procedure not materialised) */
    val context: ExecutionContext

    /** A number that changes whenever the theory of the user does; null where the solver cannot tell */
    val theoryVersion: Long?

    /** The libraries the call is made with (those of the module, within a module call) */
    val libraries: Runtime

    /** The libraries of the root of the resolution (those of the user) */
    val rootLibraries: Runtime

    /**
     * Enters [goal] against [libraries], the theory of the user visible or not, as the goal of the primitive: the
     * primitive returns what this returns ([JUMPED] or [FAIL])
     */
    fun inline(
        goal: Int,
        libraries: Runtime,
        theoryVisible: Boolean,
    ): Int

    /**
     * Enters [goal] resolved with the clauses [source] gives for it, as the goal of the primitive: the primitive
     * returns what this returns ([JUMPED], [CONTINUE] or [FAIL])
     */
    fun sourced(
        goal: Int,
        source: ClauseSource,
    ): Int

    /**
     * Leaves [resume] as the alternatives of the call: the resolution backtracking into it calls [resume] with the
     * arguments back in place (it may leave alternatives again)
     */
    fun alternatives(resume: NativeResume)

    /** [items] in the standard order of terms, those equal in the order they came; null if a variable is in them */
    fun sorted(items: IntArray): IntArray?

    /** As [sorted], the items equal to an earlier one left out */
    fun sortedDistinct(items: IntArray): IntArray?

    /** A copy of the term [w] off the heap */
    fun record(w: Int): Record

    /** [record] onto the heap, its variables fresh */
    fun recall(record: Record): Int

    /** The list of the instances of [template] for every solution of [goal], as findall/3 (the registers kept) */
    fun findall(
        template: Int,
        goal: Int,
    ): Int

    /** How many solutions [goal] has, counted up to [limit] (the registers kept) */
    fun countSolutions(
        goal: Int,
        limit: Int,
    ): Int

    companion object {
        const val FAIL = 0
        const val CONTINUE = 1
        const val JUMPED = 2

        const val VAR = 0
        const val ATOM = 1
        const val INT = 2
        const val FLOAT = 3
        const val STRUCT = 4
        const val LIST = 5
    }
}
