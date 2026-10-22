package it.unibo.tuprolog.argumentation.core.primitives.utils

import it.unibo.tuprolog.argumentation.core.runtime.Arg2pUnificator
import it.unibo.tuprolog.core.List
import it.unibo.tuprolog.core.Substitution
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.Var
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.exception.error.TypeError
import it.unibo.tuprolog.solve.primitive.BinaryRelation
import it.unibo.tuprolog.solve.primitive.Primitive
import it.unibo.tuprolog.solve.primitive.Solve
import it.unibo.tuprolog.solve.primitive.TernaryRelation

// Primitives on lists, for the module utils

object AppendOptimized : Primitive {
    val signature: Signature = Signature("append_fast", 3)

    override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
        val list1: Term = request.arguments[0]
        val list2: Term = request.arguments[1]
        val res: Term = request.arguments[2]

        if (list1 !is List) {
            throw TypeError.forGoal(
                request.context,
                request.signature,
                TypeError.Expected.LIST,
                list1,
            )
        }

        if (list2 !is List) {
            throw TypeError.forGoal(
                request.context,
                request.signature,
                TypeError.Expected.LIST,
                list2,
            )
        }

        if (res !is Var) {
            throw TypeError.forGoal(
                request.context,
                request.signature,
                TypeError.Expected.VARIABLE,
                res,
            )
        }

        return sequence {
            yield(
                request.replyWith(Substitution.of(res, List.of(list1.toList() + list2.toList()))),
            )
        }
    }
}

/** appendLists(Lists, Result): Result is the concatenation of the lists in Lists */
object AppendLists : Primitive {
    val signature: Signature = Signature("appendLists", 2)

    override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
        val lists: Term = request.arguments[0]

        if (lists !is List) {
            throw TypeError.forGoal(
                request.context,
                request.signature,
                TypeError.Expected.LIST,
                lists,
            )
        }

        val items =
            lists.toList().flatMap { list ->
                if (list !is List) {
                    throw TypeError.forGoal(
                        request.context,
                        request.signature,
                        TypeError.Expected.LIST,
                        list,
                    )
                }
                list.toList()
            }
        val unifier = Arg2pUnificator.mgu(request.arguments[1], List.of(items))
        return sequenceOf(if (unifier.isSuccess) request.replyWith(unifier) else request.replyFail())
    }
}

// intersection(+Sets, -Intersection): the elements of the first list in Sets that are in all of them, in its order
// (elements compared as ground terms); fails if Sets is empty
object Intersection : BinaryRelation.WithoutSideEffects<ExecutionContext>("intersection") {
    override fun Solve.Request<ExecutionContext>.computeAllSubstitutions(
        first: Term,
        second: Term,
    ): Sequence<Substitution> {
        if (first !is List) return emptySequence()
        val sets = first.toList().map { (it as? List)?.toList() ?: return emptySequence() }
        if (sets.isEmpty()) return emptySequence()
        val others = sets.drop(1).map { it.toSet() }
        return unifyWith(second, List.of(sets.first().filter { y -> others.all { y in it } }))
    }
}

// subtract(+List, +Removed, ?Rest): the elements of List not in Removed, in order (elements compared as ground terms),
// without the recursion of a Prolog subtract (whose frames would all keep the rest of the list)
object Subtract : TernaryRelation.WithoutSideEffects<ExecutionContext>("subtract") {
    override fun Solve.Request<ExecutionContext>.computeAllSubstitutions(
        first: Term,
        second: Term,
        third: Term,
    ): Sequence<Substitution> {
        if (first !is List || second !is List) return emptySequence()
        val removed = second.toList().toSet()
        return unifyWith(third, List.of(first.toList().filter { it !in removed }))
    }
}

object Contains : Primitive {
    val signature: Signature = Signature("contains", 2)

    override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
        val elem: Term = request.arguments[0]
        val list: Term = request.arguments[1]

        if (list !is List) {
            throw TypeError.forGoal(
                request.context,
                request.signature,
                TypeError.Expected.LIST,
                list,
            )
        }

        return sequence {
            yield(
                request.replyWith(list.toList().any { Arg2pUnificator.match(it, elem) }),
            )
        }
    }
}

object ContainsAny : Primitive {
    val signature: Signature = Signature("contains_any", 2)

    override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
        val elemList: Term = request.arguments[0]
        val list: Term = request.arguments[1]

        if (list !is List) {
            throw TypeError.forGoal(
                request.context,
                request.signature,
                TypeError.Expected.LIST,
                list,
            )
        }

        if (elemList !is List) {
            throw TypeError.forGoal(
                request.context,
                request.signature,
                TypeError.Expected.LIST,
                elemList,
            )
        }

        return sequence {
            yield(
                request.replyWith(list.toSequence().any { elemList.toSequence().any { elem -> Arg2pUnificator.match(it, elem) } }),
            )
        }
    }
}
