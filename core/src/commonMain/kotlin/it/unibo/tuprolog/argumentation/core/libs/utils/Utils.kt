package it.unibo.tuprolog.argumentation.core.libs.utils

import it.unibo.tuprolog.argumentation.core.dsl.arg2pScope
import it.unibo.tuprolog.argumentation.core.libs.ArgLibrary
import it.unibo.tuprolog.argumentation.core.libs.ArgsFlag
import it.unibo.tuprolog.argumentation.core.libs.LazyRawPrologContent
import it.unibo.tuprolog.argumentation.core.libs.Loadable
import it.unibo.tuprolog.argumentation.core.libs.basic.DynamicLoader
import it.unibo.tuprolog.core.List
import it.unibo.tuprolog.core.Numeric
import it.unibo.tuprolog.core.Substitution
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.Var
import it.unibo.tuprolog.core.operators.OperatorSet
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.exception.error.TypeError
import it.unibo.tuprolog.solve.library.AliasedLibrary
import it.unibo.tuprolog.solve.library.Library
import it.unibo.tuprolog.solve.primitive.BinaryRelation
import it.unibo.tuprolog.solve.primitive.Primitive
import it.unibo.tuprolog.solve.primitive.Solve

sealed class UtilsBase : ArgLibrary, LazyRawPrologContent(), Loadable {

    override val alias = "prolog.argumentation.utils"

    override val baseContent: AliasedLibrary
        get() = Library.aliased(
            alias = this.alias,
            theory = this.prologTheory,
            primitives = mapOf(
                AppendOptimized.signature to AppendOptimized,
                AssertAll.signature to AssertAll,
                ArgumentHash.descriptionPair
            )
        )
    override val baseFlags: Iterable<ArgsFlag<*, *>>
        get() = emptyList()

    override fun identifier(): String = "utils"

    override val theoryOperators = DynamicLoader.operators()
        .plus(OperatorSet.DEFAULT)
}

expect object Utils : UtilsBase

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
                list1
            )
        }

        if (list2 !is List) {
            throw TypeError.forGoal(
                request.context,
                request.signature,
                TypeError.Expected.LIST,
                list2
            )
        }

        if (res !is Var) {
            throw TypeError.forGoal(
                request.context,
                request.signature,
                TypeError.Expected.VARIABLE,
                res
            )
        }

        return sequence {
            yield(
                request.replyWith(Substitution.of(res, List.of(list1.toList() + list2.toList())))
            )
        }
    }
}

object AssertAll : Primitive {

    val signature: Signature = Signature("assert_all", 1)

    override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
        val list: Term = request.arguments[0]

        if (list !is List) {
            throw TypeError.forGoal(
                request.context,
                request.signature,
                TypeError.Expected.LIST,
                list
            )
        }

        arg2pScope {
            list.toList().forEach { request.solve("context_assert"(it)).first() }
        }

        return sequence {
            yield(
                request.replyWith(true)
            )
        }
    }
}

object ArgumentHash : BinaryRelation.WithoutSideEffects<ExecutionContext>("hash") {
    override fun Solve.Request<ExecutionContext>.computeAllSubstitutions(first: Term, second: Term): Sequence<Substitution> =
        sequenceOf(
            Substitution.of(
                second.asVar()!!,
                Numeric.of(first.toString().hashCode())
            )
        )
}
