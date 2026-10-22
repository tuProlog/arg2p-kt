package it.unibo.tuprolog.argumentation.core.libs.basic

import it.unibo.tuprolog.argumentation.core.engine.DelegatedCall
import it.unibo.tuprolog.argumentation.core.engine.Delegation
import it.unibo.tuprolog.argumentation.core.libs.PrimitiveWithSignature
import it.unibo.tuprolog.core.Integer
import it.unibo.tuprolog.core.List
import it.unibo.tuprolog.core.Numeric
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.Var
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.primitive.Solve

// The primitives of the context reading the clauses of its branches

// The lookup of a goal in a branch: context_check(Branch, Goal)
private fun branchLookup(branch: Int) = StoreLookup { Struct.of("context_check", Integer.of(branch), it) }

/**
 * context_check(Goal): the solutions of Goal in the active branch (see [ClauseStore.solve]); the arg2p engine
 * resolves Goal with the clauses of the branch itself (see [StoreClauses])
 */
internal class ContextCheck(
    private val branches: ContextBranches,
) : PrimitiveWithSignature,
    DelegatedCall {
    override val signature = Signature("context_check", 1)

    override fun delegate(request: Solve.Request<ExecutionContext>): Delegation? =
        StoreClauses.delegation(branches.selected, branchLookup(branches.active), request.arguments[0], request.context.libraries)

    override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> =
        branches.selected.solve(request.arguments[0]).map { request.replyWith(it) }
}

/** context_check(+Branch, Goal): the solutions of Goal in the given branch, as context_check/1 */
internal class ContextCheckIn(
    private val branches: ContextBranches,
) : PrimitiveWithSignature,
    DelegatedCall {
    override val signature = Signature("context_check", 2)

    // A branch that does not exist has no solutions (solved as the primitive does)
    override fun delegate(request: Solve.Request<ExecutionContext>): Delegation? {
        val branch = request.branchArgument(0)
        val store = branches.storeOf(branch) ?: return null
        return StoreClauses.delegation(store, branchLookup(branch), request.arguments[1], request.context.libraries)
    }

    override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> =
        branches
            .storeOf(request.branchArgument(0))
            ?.solve(request.arguments[1])
            ?.map { request.replyWith(it) }
            ?: emptySequence()
}

/**
 * context_all(+Goal, -Instances): the instances of Goal in the active branch as one list, `findall` as a single step.
 * context_all(+Branch, +Goal, -Instances): the same in the given branch.
 */
internal class ContextAll(
    arity: Int,
    private val branches: ContextBranches,
    // The context this primitive reads, for the native primitives of the other libraries (see Context.of)
    val owner: Context,
) : PrimitiveWithSignature {
    override val signature = Signature("context_all", arity)

    override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
        val store =
            if (request.arguments.size == 2) {
                branches.selected
            } else {
                branches.storeOf(request.branchArgument(0)) ?: return sequenceOf(request.replyFail())
            }
        val goal = request.arguments[request.arguments.size - 2]
        val instances = store.solve(goal).map { goal.apply(it) }.toList()
        return sequenceOf(request.replyUnifying(request.arguments.size - 1, List.of(instances)))
    }
}

/**
 * context_count(+Goal, +Limit, -Count): the number of solutions of Goal in the active branch, counted up to Limit
 * (an integer, or anything else for no limit): enough to compare how selective goals are, without visiting all
 * the solutions of the least selective ones
 */
internal class ContextCount(
    private val branches: ContextBranches,
) : PrimitiveWithSignature {
    override val signature = Signature("context_count", 3)

    override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
        val limit =
            request.arguments[1]
                .asInteger()
                ?.intValue
                ?.toInt() ?: Int.MAX_VALUE
        val count =
            branches.selected
                .solve(request.arguments[0])
                .take(limit)
                .count()
        return sequenceOf(request.replyUnifying(2, Numeric.of(count)))
    }
}

/** context_absent(Goal): `\+ context_check(Goal)` as a single step */
internal class ContextAbsent(
    private val branches: ContextBranches,
) : PrimitiveWithSignature {
    override val signature = Signature("context_absent", 1)

    override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> =
        sequenceOf(request.replyWith(branches.selected.solve(request.arguments[0]).none()))
}

/** context_absent(+Branch, Goal): `\+ context_check(Branch, Goal)` as a single step; a branch that does not exist has none */
internal class ContextAbsentIn(
    private val branches: ContextBranches,
) : PrimitiveWithSignature {
    override val signature = Signature("context_absent", 2)

    override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
        val store = branches.storeOf(request.branchArgument(0))
        return sequenceOf(request.replyWith(store?.solve(request.arguments[1])?.none() ?: true))
    }
}

/**
 * context_closure(+Functor, +From, ?To): To is reachable from From through one or more Functor(X, Y) facts,
 * each reachable term once (breadth first)
 */
internal class ContextClosure(
    private val branches: ContextBranches,
) : PrimitiveWithSignature {
    override val signature = Signature("context_closure", 3)

    override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
        val functor = request.arguments[0].castToAtom().value
        val store = branches.selected
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
internal class ContextUnifiable(
    private val branches: ContextBranches,
) : PrimitiveWithSignature {
    override val signature = Signature("context_unifiable", 2)

    override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> =
        branches.selected.unifiableBodies(request.arguments[0], request.arguments[1]).map { request.replyWith(it) }
}

/** context_id(+Term, ?Id): the integer identifier of Term (see [ContextBranches.identifier]) */
internal class ContextIdentifier(
    private val branches: ContextBranches,
) : PrimitiveWithSignature {
    override val signature = Signature("context_id", 2)

    override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> =
        sequenceOf(request.replyUnifying(1, Numeric.of(branches.identifier(request.arguments[0]))))
}
