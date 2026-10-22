package it.unibo.tuprolog.argumentation.core.libs.basic

import it.unibo.tuprolog.argumentation.core.libs.ArgLibrary
import it.unibo.tuprolog.argumentation.core.libs.ArgsFlag
import it.unibo.tuprolog.argumentation.core.libs.PrimitiveWithSignature
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.library.Library
import it.unibo.tuprolog.solve.primitive.Solve

// Facts shared across context branches, kept in the same indexed store as the context
class Cache : ArgLibrary {
    private val store = ClauseStore()

    inner class CacheAssert : PrimitiveWithSignature {
        override val signature = Signature("cache_assert", 1)

        override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
            val term: Term = request.arguments[0]
            this@Cache.store.assertA(term.castToStruct())
            return sequenceOf(request.replyWith(true))
        }
    }

    inner class CacheRetract : PrimitiveWithSignature {
        override val signature = Signature("cache_retract", 1)

        override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
            val term: Term = request.arguments[0]
            this@Cache.store.retractAll(term.castToStruct())
            return sequenceOf(request.replyWith(true))
        }
    }

    inner class CacheGet : PrimitiveWithSignature {
        override val signature = Signature("cache_check", 1)

        override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
            val term: Term = request.arguments[0]
            return this@Cache.store.solve(term.castToStruct()).map { request.replyWith(it) }
        }
    }

    override val alias = "prolog.argumentation.cache"

    override val baseContent: Library
        get() =
            listOf(
                CacheAssert(),
                CacheRetract(),
                CacheGet(),
            ).let { primitives ->
                Library.of(
                    alias = this.alias,
                    primitives = primitives.associateBy { it.signature },
                )
            }

    override val baseFlags: Iterable<ArgsFlag<*, *>>
        get() = emptyList()
}
