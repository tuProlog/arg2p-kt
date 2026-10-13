package it.unibo.tuprolog.argumentation.core.libs.basic

import it.unibo.tuprolog.argumentation.core.libs.ArgContext
import it.unibo.tuprolog.argumentation.core.libs.ArgLibrary
import it.unibo.tuprolog.argumentation.core.libs.ArgsFlag
import it.unibo.tuprolog.argumentation.core.libs.PrimitiveWithSignature
import it.unibo.tuprolog.core.Atom
import it.unibo.tuprolog.core.Numeric
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Substitution
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.Var
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.library.Library
import it.unibo.tuprolog.solve.primitive.Solve
import it.unibo.tuprolog.unify.Unificator

class Context :
    ArgLibrary,
    ArgContext {
    private var nextSolver: Int = 1
    private var selectedSolver: Int = 0
    private val dynamicSolver: MutableMap<Int, ClauseStore> = mutableMapOf(0 to ClauseStore())
    private val identifiers: MutableMap<Term, Int> = mutableMapOf()

    private val selected: ClauseStore
        get() = dynamicSolver[selectedSolver]!!

    inner class DynamicCacheReset : PrimitiveWithSignature {
        override val signature = Signature("context_reset", 0)

        override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
            this@Context.selectedSolver = 0
            this@Context.nextSolver = 1
            this@Context.dynamicSolver.clear()
            this@Context.dynamicSolver[0] = ClauseStore()
            this@Context.identifiers.clear()
            return sequenceOf(request.replyWith(true))
        }
    }

    inner class DynamicCacheSelected : PrimitiveWithSignature {
        override val signature = Signature("context_active", 1)

        override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
            val term: Term = request.arguments[0]
            return sequenceOf(request.replyWith(Substitution.of(term.castToVar(), Numeric.of(this@Context.selectedSolver))))
        }
    }

    inner class DynamicCacheCheckout : PrimitiveWithSignature {
        override val signature = Signature("context_checkout", 1)

        override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
            val target: Int =
                request.arguments[0]
                    .castToInteger()
                    .intValue
                    .toInt()
            val result = dynamicSolver.keys.contains(target)
            this@Context.selectedSolver = target
            return sequenceOf(request.replyWith(result))
        }
    }

    inner class DynamicCacheBranch : PrimitiveWithSignature {
        override val signature = Signature("context_branch", 2)

        override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
            val target: Int =
                request.arguments[0]
                    .castToInteger()
                    .intValue
                    .toInt()
            val result: Term = request.arguments[1]
            this@Context.dynamicSolver[this@Context.nextSolver] = this@Context.dynamicSolver[target]!!.fork()
            this@Context.selectedSolver = this@Context.nextSolver++
            return sequenceOf(request.replyWith(Substitution.of(result.castToVar(), Numeric.of(this@Context.selectedSolver))))
        }
    }

    inner class DynamicCacheAssert : PrimitiveWithSignature {
        override val signature = Signature("context_assert", 1)

        override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
            this@Context.selected.assertA(request.arguments[0])
            return sequenceOf(request.replyWith(true))
        }
    }

    inner class DynamicCacheRetract : PrimitiveWithSignature {
        override val signature = Signature("context_retract", 1)

        override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
            this@Context.selected.retractAll(request.arguments[0])
            return sequenceOf(request.replyWith(true))
        }
    }

    inner class DynamicCacheGet : PrimitiveWithSignature {
        override val signature = Signature("context_check", 1)

        override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> =
            this@Context.selected.solve(request.arguments[0]).map { request.replyWith(it) }
    }

    inner class DynamicCacheGetIndexed : PrimitiveWithSignature {
        override val signature = Signature("context_check", 2)

        override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
            val index: Int =
                request.arguments[0]
                    .asNumeric()!!
                    .intValue
                    .toInt()
            return this@Context.dynamicSolver[index]!!.solve(request.arguments[1]).map { request.replyWith(it) }
        }
    }

    /** context_absent(Goal): `\+ context_check(Goal)` as a single step */
    inner class DynamicCacheAbsent : PrimitiveWithSignature {
        override val signature = Signature("context_absent", 1)

        override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> =
            sequenceOf(request.replyWith(this@Context.selected.solve(request.arguments[0]).none()))
    }

    /** context_absent(Branch, Goal): `\+ context_check(Branch, Goal)` as a single step */
    inner class DynamicCacheAbsentIndexed : PrimitiveWithSignature {
        override val signature = Signature("context_absent", 2)

        override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
            val index: Int =
                request.arguments[0]
                    .asNumeric()!!
                    .intValue
                    .toInt()
            return sequenceOf(request.replyWith(this@Context.dynamicSolver[index]!!.solve(request.arguments[1]).none()))
        }
    }

    /**
     * context_closure(+Functor, +From, ?To): To is reachable from From through one or more Functor(X, Y) facts,
     * each reachable term once (breadth first)
     */
    inner class DynamicCacheClosure : PrimitiveWithSignature {
        override val signature = Signature("context_closure", 3)

        override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
            val functor = request.arguments[0].castToAtom().value
            val store = this@Context.selected
            val next = Var.of("Next")
            val visited = LinkedHashSet<Term>()
            val queue = ArrayDeque<Term>().apply { addLast(request.arguments[1]) }
            while (queue.isNotEmpty()) {
                val current = queue.removeFirst()
                store.solve(Struct.of(functor, current, next)).forEach { solution ->
                    val reached = solution[next] ?: return@forEach
                    if (visited.add(reached)) queue.addLast(reached)
                }
            }
            val target = request.arguments[2]
            return visited.asSequence().mapNotNull { reached ->
                Unificator.default
                    .mgu(target, reached)
                    .takeIf { it.isSuccess }
                    ?.let { request.replyWith(it) }
            }
        }
    }

    /** context_unifiable(Head, Body): Body of the stored clauses whose head unifies with Head, without binding them */
    inner class DynamicCacheUnifiable : PrimitiveWithSignature {
        override val signature = Signature("context_unifiable", 2)

        override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> =
            this@Context.selected.unifiableBodies(request.arguments[0], request.arguments[1]).map { request.replyWith(it) }
    }

    /**
     * Integer identifier of a term, shared by all the copies of the term which only differ in variable names
     * (variables get renamed whenever a term is read back from the context)
     */
    inner class DynamicCacheIdentifier : PrimitiveWithSignature {
        override val signature = Signature("context_id", 2)

        override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
            val id = this@Context.identifiers.getOrPut(canonical(request.arguments[0])) { this@Context.identifiers.size }
            val unifier = Unificator.default.mgu(request.arguments[1], Numeric.of(id))
            return sequenceOf(if (unifier.isSuccess) request.replyWith(unifier) else request.replyFail())
        }

        private fun canonical(term: Term): Term {
            val variables = term.variables.distinct().toList()
            if (variables.isEmpty()) return term
            return term.apply(Substitution.of(variables.mapIndexed { i, v -> v to Atom.of("_V$i") }.toMap()))
        }
    }

    override val alias = "prolog.argumentation.context"

    override val baseContent: Library
        get() =
            listOf(
                DynamicCacheReset(),
                DynamicCacheCheckout(),
                DynamicCacheSelected(),
                DynamicCacheBranch(),
                DynamicCacheAssert(),
                DynamicCacheRetract(),
                DynamicCacheGet(),
                DynamicCacheGetIndexed(),
                DynamicCacheIdentifier(),
                DynamicCacheUnifiable(),
                DynamicCacheAbsent(),
                DynamicCacheAbsentIndexed(),
                DynamicCacheClosure(),
            ).let { primitives ->
                Library.of(
                    alias = this.alias,
                    primitives = primitives.associateBy { it.signature },
                )
            }

    override val baseFlags: Iterable<ArgsFlag<*, *>>
        get() = emptyList()
}
