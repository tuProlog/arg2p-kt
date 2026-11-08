package it.unibo.tuprolog.argumentation.engine.vm

import it.unibo.tuprolog.argumentation.engine.term.haltsRaised
import it.unibo.tuprolog.argumentation.engine.term.renamedCopy
import it.unibo.tuprolog.argumentation.engine.term.solveForCaller
import it.unibo.tuprolog.core.Atom
import it.unibo.tuprolog.core.Cons
import it.unibo.tuprolog.core.Integer
import it.unibo.tuprolog.core.Real
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Substitution
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.Var
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.Solution
import it.unibo.tuprolog.solve.SolveOptions
import it.unibo.tuprolog.solve.library.Runtime
import it.unibo.tuprolog.solve.primitive.Solve
import it.unibo.tuprolog.theory.Theory
import it.unibo.tuprolog.core.List as LogicList

/**
 * A [NativeCall] over the terms of a request, for the solvers that are not the machine: the words are the terms
 * made or read so far, read under the substitution the unifications built; a goal entered is solved in a solver
 * of its own once the primitive returns; the alternatives are run on a fresh call each
 */
internal class TermCall(
    private val request: Solve.Request<ExecutionContext>,
) : NativeCall {
    private val terms = ArrayList<Term>()

    /** The bindings made so far */
    var substitution: Substitution.Unifier = Substitution.empty()
        private set

    /** The alternatives left, if any */
    var resume: NativeResume? = null
        private set

    // A goal entered: against the libraries, the theory hidden or not
    private class Entered(
        val goal: Struct,
        val libraries: Runtime,
        val theory: Boolean,
    )

    private var entered: Entered? = null

    private fun put(term: Term): Int {
        terms.add(term)
        return terms.size - 1
    }

    private fun at(w: Int): Term = terms[w].apply(substitution)

    override val arity: Int get() = request.arguments.size

    override fun arg(i: Int): Int = put(request.arguments[i].apply(substitution))

    override fun deref(w: Int): Int = put(at(w))

    override fun kind(w: Int): Int =
        when (val term = at(w)) {
            is Var -> NativeCall.VAR
            is Integer -> NativeCall.INT
            is Real -> NativeCall.FLOAT
            is Atom -> NativeCall.ATOM
            is Cons -> NativeCall.LIST
            is Struct -> NativeCall.STRUCT
            else -> throw IllegalStateException("not a term of the machine: $term")
        }

    override fun atomName(w: Int): String = at(w).castToAtom().value

    override fun intValue(w: Int): Long = at(w).castToInteger().value.toLong()

    override fun functorName(w: Int): String = at(w).castToStruct().functor

    override fun arityOf(w: Int): Int = at(w).castToStruct().arity

    override fun argOf(
        w: Int,
        i: Int,
    ): Int = put(at(w).castToStruct()[i])

    override fun unify(
        a: Int,
        b: Int,
    ): Boolean {
        val mgu = request.context.unificator.mgu(at(a), at(b))
        if (!mgu.isSuccess) return false
        val composed = substitution + mgu
        if (!composed.isSuccess) return false
        substitution = composed.castToUnifier()
        return true
    }

    override fun unifiable(
        a: Int,
        b: Int,
    ): Boolean =
        request.context.unificator
            .mgu(at(a), at(b))
            .isSuccess

    override fun identical(
        a: Int,
        b: Int,
    ): Boolean = at(a) == at(b)

    override fun hashOf(w: Int): Int = at(w).hashCode()

    override fun atom(name: String): Int = put(Atom.of(name))

    override fun int(value: Long): Int = put(Integer.of(value))

    override fun struct(
        name: String,
        args: IntArray,
    ): Int = put(Struct.of(name, args.map { at(it) }))

    override fun list(
        items: IntArray,
        tail: Int,
    ): Int = put(LogicList.from(items.map { at(it) }, at(tail)))

    override val emptyList: Int get() = put(LogicList.empty())

    override fun variable(): Int = put(Var.anonymous())

    override fun term(w: Int): Term = at(w)

    override fun word(term: Term): Int = put(term)

    override val context: ExecutionContext get() = request.context

    override val theoryVersion: Long? get() = null

    override val libraries: Runtime get() = request.context.libraries

    override val rootLibraries: Runtime
        get() = (request.context as? PrologExecutionContext)?.root?.libraries ?: request.context.libraries

    override fun inline(
        goal: Int,
        libraries: Runtime,
        theoryVisible: Boolean,
    ): Int {
        entered = Entered(at(goal).castToStruct(), libraries, theoryVisible)
        return NativeCall.JUMPED
    }

    override fun alternatives(resume: NativeResume) {
        this.resume = resume
    }

    override fun sorted(items: IntArray): IntArray? = null

    override fun sortedDistinct(items: IntArray): IntArray? = null

    override fun record(w: Int): Record = Record.of(at(w))

    override fun recall(record: Record): Int = put(record.term.renamedCopy())

    override fun findall(
        template: Int,
        goal: Int,
    ): Int {
        val instance = at(template)
        // (solved for the caller: the bindings of the anonymous variables of the goal are the caller's too)
        val items =
            request.context
                .createSolver()
                .solveForCaller(at(goal).castToStruct(), SolveOptions.allLazily())
                .haltsRaised()
                .filterIsInstance<Solution.Yes>()
                .map { instance.apply(it.substitution) }
                .toList()
        return put(LogicList.of(items))
    }

    override fun countSolutions(
        goal: Int,
        limit: Int,
    ): Int =
        request.context
            .createSolver()
            .solveForCaller(at(goal).castToStruct(), SolveOptions.allLazily())
            .haltsRaised()
            .filterIsInstance<Solution.Yes>()
            .take(limit)
            .count()

    // The answers of the goal entered: solved in a solver of the libraries (the theory of the caller kept or not)
    private fun answersOfEntered(): Sequence<Solve.Response> {
        val entered = entered!!
        val context = request.context
        val solver =
            if (entered.theory) {
                context.createSolver(libraries = entered.libraries)
            } else {
                context.createSolver(libraries = entered.libraries, staticKb = Theory.empty(), dynamicKb = Theory.empty())
            }
        val remaining =
            (
                context.maxDuration - (
                    it.unibo.tuprolog.solve
                        .currentTimeInstant() - context.startTime
                )
            ).coerceAtLeast(0)
        return solver
            .solveForCaller(entered.goal, SolveOptions.allLazilyWithTimeout(remaining))
            .haltsRaised()
            .map { solution ->
                when (solution) {
                    is Solution.Yes -> {
                        val composed = substitution + solution.substitution
                        if (composed.isSuccess) request.replyWith(composed.castToUnifier()) else request.replyFail()
                    }
                    else -> request.replyFail()
                }
            }
    }

    companion object {
        /** The responses of [primitive] to [request], run over the terms of the request */
        fun solve(
            primitive: NativePrimitive,
            request: Solve.Request<ExecutionContext>,
        ): Sequence<Solve.Response> =
            sequence {
                var step: (NativeCall) -> Int = primitive::call
                while (true) {
                    val call = TermCall(request)
                    when (step(call)) {
                        NativeCall.CONTINUE -> yield(request.replyWith(call.substitution))
                        NativeCall.JUMPED -> yieldAll(call.answersOfEntered())
                        else -> if (call.resume == null) yield(request.replyFail())
                    }
                    val next = call.resume ?: break
                    step = { next.resume(it) }
                }
            }
    }
}
