package it.unibo.tuprolog.argumentation.core.engine

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

    fun atom(name: String): Int

    fun int(value: Long): Int

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

    /** A copy of the term [w] off the heap */
    fun record(w: Int): Record

    /** [record] onto the heap, its variables fresh */
    fun recall(record: Record): Int

    /**
     * Enters [goal] resolved with [records] as the clauses of its predicate (their heads unified with its arguments,
     * their bodies called), the goals of a body each wrapped by [wrap] (null: called as they are), as the goal of the
     * primitive: the primitive returns what this returns
     */
    fun sourcedRecords(
        goal: Int,
        records: List<Record>,
        wrap: GoalWrapper?,
    ): Int

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
