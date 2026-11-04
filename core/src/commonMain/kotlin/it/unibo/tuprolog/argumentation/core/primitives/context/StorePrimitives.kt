package it.unibo.tuprolog.argumentation.core.primitives.context

import it.unibo.tuprolog.argumentation.core.engine.GoalWrapper
import it.unibo.tuprolog.argumentation.core.engine.NativeCall
import it.unibo.tuprolog.argumentation.core.engine.items
import it.unibo.tuprolog.argumentation.core.engine.vm.Machine
import it.unibo.tuprolog.argumentation.core.runtime.context.ClauseStore
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.exception.error.TypeError

// The lookups of the primitives of the stores (the branches of the context, the cache), as the machine runs them

/** The lookup of a goal in a branch: context_check(Branch, Goal) */
internal fun branchLookup(branch: Int) =
    GoalWrapper { call, goal -> call.struct("context_check", intArrayOf(call.int(branch.toLong()), goal)) }

/**
 * The lookup of the argument [i] in [store]: a conjunction as the lookup of each goal (through [wrap]), a
 * `clause(Head, Body)` goal as the clauses stored for Head, any other goal entered with the records stored for it
 * (the goals of their bodies looked up through [wrap])
 */
internal fun NativeCall.lookup(
    store: ClauseStore,
    wrap: GoalWrapper,
    i: Int,
): Int {
    val goal = arg(i)
    return when (kind(goal)) {
        NativeCall.VAR, NativeCall.INT, NativeCall.FLOAT -> NativeCall.FAIL
        NativeCall.ATOM -> if (atomName(goal) == "true") NativeCall.CONTINUE else enter(store, wrap, goal)
        NativeCall.STRUCT ->
            when {
                functorName(goal) == "," && arityOf(goal) == 2 -> inline(wrappedConjunction(goal, wrap), libraries, theoryVisible = false)
                functorName(goal) == "clause" && arityOf(goal) == 2 -> clauseLookup(store, goal)
                else -> enter(store, wrap, goal)
            }
        else -> enter(store, wrap, goal)
    }
}

private fun NativeCall.enter(
    store: ClauseStore,
    wrap: GoalWrapper,
    goal: Int,
): Int = (this as Machine).storeGoal(goal, store, wrap)

// The goals of a conjunction each wrapped
private fun NativeCall.wrappedConjunction(
    goal: Int,
    wrap: GoalWrapper,
): Int =
    when {
        kind(goal) == NativeCall.STRUCT && functorName(goal) == "," && arityOf(goal) == 2 ->
            struct(",", intArrayOf(wrappedConjunction(argOf(goal, 0), wrap), wrappedConjunction(argOf(goal, 1), wrap)))
        kind(goal) == NativeCall.ATOM && atomName(goal) == "true" -> goal
        else -> wrap.wrap(this, goal)
    }

// clause(Head, Body): the clauses stored for Head that unify with them
private fun NativeCall.clauseLookup(
    store: ClauseStore,
    goal: Int,
): Int {
    val head = argOf(goal, 0)
    return if (isCallable(head)) (this as Machine).storeClause(head, argOf(goal, 1), store) else NativeCall.FAIL
}

/**
 * Whether the goal [goal] has no answer in [store], asked without entering it where only facts or clause heads can
 * answer; null when a rule may answer: its body is to be solved
 */
internal fun NativeCall.absentFrom(
    store: ClauseStore,
    goal: Int,
): Boolean? {
    val machine = this as? Machine ?: return null
    val isClause = kind(goal) == NativeCall.STRUCT && functorName(goal) == "clause" && arityOf(goal) == 2
    return when {
        isClause -> {
            val head = argOf(goal, 0)
            if (!isCallable(head)) true else machine.storeHolds(head, argOf(goal, 1), store)?.not()
        }
        kind(goal) == NativeCall.ATOM && atomName(goal) == "true" -> false
        kind(goal) == NativeCall.STRUCT && functorName(goal) == "," && arityOf(goal) == 2 -> null
        isCallable(goal) -> machine.storeHolds(goal, null, store)?.not()
        else -> true
    }
}

/** The head of the record recalled as [w] */
internal fun NativeCall.headOf(
    record: it.unibo.tuprolog.argumentation.core.engine.Record,
    w: Int,
): Int = if (record.isRule) argOf(w, 0) else w

/** The body of the record recalled as [w]: `true` for a fact */
internal fun NativeCall.bodyOf(
    record: it.unibo.tuprolog.argumentation.core.engine.Record,
    w: Int,
): Int = if (record.isRule) argOf(w, 1) else atom("true")

/** The records of [store] whose head may unify with the head of the clause pattern [pattern] (`Head :- Body` or Head) */
internal fun NativeCall.candidatesOf(
    store: ClauseStore,
    pattern: Int,
): List<ClauseStore.Stored> {
    val head =
        if (kind(pattern) == NativeCall.STRUCT &&
            functorName(pattern) == ":-" &&
            arityOf(pattern) == 2
        ) {
            argOf(pattern, 0)
        } else {
            pattern
        }
    if (kind(head) == NativeCall.VAR) return emptyList()
    return store.candidates(record(head))
}

/** The stored clauses unifying with the pattern [pattern] removed from [store], as retractall/1 */
internal fun NativeCall.retractAll(
    store: ClauseStore,
    pattern: Int,
) {
    val isRule = kind(pattern) == NativeCall.STRUCT && functorName(pattern) == ":-" && arityOf(pattern) == 2
    val matching =
        candidatesOf(store, pattern).filter { stored ->
            val w = recall(stored.record)
            val against =
                if (!isRule) {
                    headOf(stored.record, w)
                } else if (stored.record.isRule) {
                    w
                } else {
                    struct(":-", intArrayOf(w, atom("true")))
                }
            unifiable(pattern, against)
        }
    store.removeAll(matching)
}

/** The branches the list argument [i] names (a type error otherwise, from the primitive of [signature]) */
internal fun NativeCall.branchesArg(
    i: Int,
    signature: Signature,
): Set<Int> {
    val items = items(arg(i)) ?: throw TypeError.forArgument(context, signature, TypeError.Expected.LIST, term(arg(i)), i)
    return items
        .map { w: Int ->
            if (kind(w) == NativeCall.INT) {
                intValue(w).toInt()
            } else {
                throw TypeError.forArgument(context, signature, TypeError.Expected.INTEGER, term(w), i)
            }
        }.toSet()
}
