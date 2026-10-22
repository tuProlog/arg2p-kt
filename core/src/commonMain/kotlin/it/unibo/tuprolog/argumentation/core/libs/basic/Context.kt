package it.unibo.tuprolog.argumentation.core.libs.basic

import it.unibo.tuprolog.argumentation.core.engine.DelegatedCall
import it.unibo.tuprolog.argumentation.core.engine.Delegation
import it.unibo.tuprolog.argumentation.core.libs.ArgContext
import it.unibo.tuprolog.argumentation.core.libs.ArgLibrary
import it.unibo.tuprolog.argumentation.core.libs.ArgsFlag
import it.unibo.tuprolog.argumentation.core.libs.PrimitiveWithSignature
import it.unibo.tuprolog.core.Atom
import it.unibo.tuprolog.core.List
import it.unibo.tuprolog.core.Numeric
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Substitution
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.Var
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.library.Library
import it.unibo.tuprolog.solve.library.Runtime
import it.unibo.tuprolog.solve.primitive.Solve

class Context :
    ArgLibrary,
    ArgContext {
    private var nextSolver: Int = 1
    private var selectedSolver: Int = 0
    private val dynamicSolver: MutableMap<Int, ClauseStore> = mutableMapOf(0 to ClauseStore())
    private val identifiers: MutableMap<Term, Int> = mutableMapOf()

    private val selected: ClauseStore
        get() = dynamicSolver[selectedSolver]!!

    // Operations of the context for the native primitives of the other libraries, which reach it through [of]
    // rather than solving goals (a solver is set up to solve a goal)

    /** The active branch, as context_active/1 */
    val active: Int
        get() = selectedSolver

    /** A new branch of the given one, made active, as context_branch/2 */
    fun branch(from: Int): Int {
        dynamicSolver[nextSolver] = dynamicSolver[from]!!.fork()
        selectedSolver = nextSolver++
        return selectedSolver
    }

    /** The terms asserted in the active branch, in order, as context_assert_all/1 */
    fun assertAll(terms: Iterable<Term>) = terms.forEach { selected.assertA(it) }

    /**
     * The instances of goal in the given branch (the active one if null), as context_all: for the native primitives
     * reading the context, without solving a goal (see [DynamicCacheAll.owner])
     */
    fun instances(
        goal: Term,
        branch: Int? = null,
    ): kotlin.collections.List<Term> {
        val store = if (branch == null) selected else dynamicSolver[branch] ?: return emptyList()
        return store.solve(goal).map { goal.apply(it) }.toList()
    }

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
            return sequenceOf(request.replyWith(Substitution.of(result.castToVar(), Numeric.of(branch(target)))))
        }
    }

    /** context_drop(+Branch): frees a branch no longer needed, fails on the active one */
    inner class DynamicCacheDrop : PrimitiveWithSignature {
        override val signature = Signature("context_drop", 1)

        override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
            val target: Int =
                request.arguments[0]
                    .castToInteger()
                    .intValue
                    .toInt()
            if (target == this@Context.selectedSolver) return sequenceOf(request.replyFail())
            this@Context.dynamicSolver.remove(target)
            return sequenceOf(request.replyWith(true))
        }
    }

    /** context_last(-Branch): the most recently created branch */
    inner class DynamicCacheLast : PrimitiveWithSignature {
        override val signature = Signature("context_last", 1)

        override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> =
            sequenceOf(request.replyWith(Substitution.of(request.arguments[0].castToVar(), Numeric.of(this@Context.nextSolver - 1))))
    }

    /** context_drop_after(+Branch, +Keep): drops the branches created after Branch, except those in Keep and the active one */
    inner class DynamicCacheDropAfter : PrimitiveWithSignature {
        override val signature = Signature("context_drop_after", 2)

        override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
            val after =
                request.arguments[0]
                    .castToInteger()
                    .intValue
                    .toInt()
            val keep =
                request.arguments[1]
                    .castToList()
                    .toList()
                    .map { it.castToInteger().intValue.toInt() }
                    .toSet()
            this@Context.dynamicSolver.keys.removeAll { it > after && it !in keep && it != this@Context.selectedSolver }
            return sequenceOf(request.replyWith(true))
        }
    }

    inner class DynamicCacheAssert : PrimitiveWithSignature {
        override val signature = Signature("context_assert", 1)

        override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
            this@Context.selected.assertA(request.arguments[0])
            return sequenceOf(request.replyWith(true))
        }
    }

    /** context_assert_all(+Terms): context_assert/1 of every term in the list, in order */
    inner class DynamicCacheAssertAll : PrimitiveWithSignature {
        override val signature = Signature("context_assert_all", 1)

        override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
            assertAll(request.arguments[0].castToList().toList())
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

    // context_check(Goal): the solutions of Goal in the active branch (see ClauseStore.solve); the arg2p engine
    // resolves Goal with the clauses of the branch itself (see BranchClauses)
    inner class DynamicCacheGet :
        PrimitiveWithSignature,
        DelegatedCall {
        override val signature = Signature("context_check", 1)

        override fun delegate(request: Solve.Request<ExecutionContext>): Delegation? =
            BranchClauses.delegation(selected, selectedSolver, request.arguments[0], request.context.libraries)

        override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> =
            this@Context.selected.solve(request.arguments[0]).map { request.replyWith(it) }
    }

    // context_check(Branch, Goal): the solutions of Goal in the given branch, as context_check/1
    inner class DynamicCacheGetIndexed :
        PrimitiveWithSignature,
        DelegatedCall {
        override val signature = Signature("context_check", 2)

        override fun delegate(request: Solve.Request<ExecutionContext>): Delegation? {
            val branch = branchOf(request)
            return BranchClauses.delegation(dynamicSolver[branch]!!, branch, request.arguments[1], request.context.libraries)
        }

        override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> =
            this@Context.dynamicSolver[branchOf(request)]!!.solve(request.arguments[1]).map { request.replyWith(it) }

        private fun branchOf(request: Solve.Request<ExecutionContext>): Int =
            request.arguments[0]
                .asNumeric()!!
                .intValue
                .toInt()
    }

    /**
     * context_all(+Goal, -Instances): the instances of Goal in the active branch as one list, `findall` as a single step.
     * context_all(+Branch, +Goal, -Instances): the same in the given branch.
     */
    inner class DynamicCacheAll(
        arity: Int,
    ) : PrimitiveWithSignature {
        override val signature = Signature("context_all", arity)

        // The context this primitive reads, for the native primitives of the other libraries
        val owner: Context
            get() = this@Context

        override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
            val store =
                if (request.arguments.size == 2) {
                    this@Context.selected
                } else {
                    this@Context.dynamicSolver[
                        request.arguments[0]
                            .castToInteger()
                            .intValue
                            .toInt(),
                    ]!!
                }
            val goal = request.arguments[request.arguments.size - 2]
            val instances = store.solve(goal).map { goal.apply(it) }.toList()
            val unifier = Arg2pUnificator.mgu(request.arguments.last(), List.of(instances))
            return sequenceOf(if (unifier.isSuccess) request.replyWith(unifier) else request.replyFail())
        }
    }

    /**
     * context_count(+Goal, +Limit, -Count): the number of solutions of Goal in the active branch, counted up to Limit
     * (an integer, or anything else for no limit): enough to compare how selective goals are, without visiting all
     * the solutions of the least selective ones
     */
    inner class DynamicCacheCount : PrimitiveWithSignature {
        override val signature = Signature("context_count", 3)

        override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
            val limit =
                request.arguments[1]
                    .asInteger()
                    ?.intValue
                    ?.toInt() ?: Int.MAX_VALUE
            val store = this@Context.selected
            val count = store.solve(request.arguments[0]).take(limit).count()
            val unifier = Arg2pUnificator.mgu(request.arguments[2], Numeric.of(count))
            return sequenceOf(if (unifier.isSuccess) request.replyWith(unifier) else request.replyFail())
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
                Arg2pUnificator
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
            val unifier = Arg2pUnificator.mgu(request.arguments[1], Numeric.of(id))
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
                DynamicCacheDrop(),
                DynamicCacheDropAfter(),
                DynamicCacheLast(),
                DynamicCacheAssert(),
                DynamicCacheAssertAll(),
                DynamicCacheRetract(),
                DynamicCacheGet(),
                DynamicCacheGetIndexed(),
                DynamicCacheAll(2),
                DynamicCacheAll(3),
                DynamicCacheIdentifier(),
                DynamicCacheUnifiable(),
                DynamicCacheAbsent(),
                DynamicCacheCount(),
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

    companion object {
        /** The context library of the solver of the given context, if it is reachable (through context_all/2) */
        fun of(context: ExecutionContext): Context? = of(context.libraries)

        /** The context library among the given libraries, if any (through context_all/2) */
        fun of(libraries: Runtime): Context? = (libraries.primitives[CONTEXT_ALL] as? DynamicCacheAll)?.owner

        private val CONTEXT_ALL = Signature("context_all", 2)
    }
}
