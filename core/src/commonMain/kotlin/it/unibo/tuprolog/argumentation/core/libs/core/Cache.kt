package it.unibo.tuprolog.argumentation.core.libs.core

import it.unibo.tuprolog.argumentation.core.libs.ArgsFlag
import it.unibo.tuprolog.argumentation.core.libs.BaseArgLibrary
import it.unibo.tuprolog.core.Numeric
import it.unibo.tuprolog.core.Substitution
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.MutableSolver
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.flags.Unknown
import it.unibo.tuprolog.solve.library.AliasedLibrary
import it.unibo.tuprolog.solve.library.Library
import it.unibo.tuprolog.solve.primitive.Primitive
import it.unibo.tuprolog.solve.primitive.Solve
import it.unibo.tuprolog.theory.MutableTheory
import it.unibo.tuprolog.theory.Theory

interface PrimitiveWithSignature : Primitive {
    val signature : Signature
}

class Cache : BaseArgLibrary() {

    private val solver : MutableSolver =
        MutableSolver.classic.mutableSolverOf(Theory.empty(), MutableTheory.empty())
            .also { it.setFlag(Unknown.name, Unknown.FAIL) }

    private var nextSolver : Int = 1
    private var selectedSolver : Int = 0
    private val dynamicSolver : MutableMap<Int, MutableSolver> = mutableMapOf(0 to
            MutableSolver.classic.mutableSolverOf(Theory.empty(), MutableTheory.empty())
                .also { it.setFlag(Unknown.name, Unknown.FAIL) })

    inner class DynamicCacheReset : PrimitiveWithSignature {

        override val signature = Signature("context_reset", 0)

        override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
            this@Cache.selectedSolver = 0
            this@Cache.nextSolver = 1
            this@Cache.dynamicSolver.clear()
            this@Cache.dynamicSolver[0] =
                MutableSolver.classic.mutableSolverOf(Theory.empty(), MutableTheory.empty())
                    .also { it.setFlag(Unknown.name, Unknown.FAIL) }
            return sequenceOf(request.replyWith(true))
        }
    }

    inner class DynamicCacheSelected : PrimitiveWithSignature {

        override val signature = Signature("context_active", 1)

        override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
            val term: Term = request.arguments[0]
            return sequenceOf(request.replyWith(Substitution.of(term.castToVar(), Numeric.of(this@Cache.selectedSolver))))
        }
    }

    inner class DynamicCacheCheckout : PrimitiveWithSignature {

        override val signature = Signature("context_checkout", 1)

        override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
            val target: Int = request.arguments[0].castToInteger().intValue.toInt()
            val result = dynamicSolver.keys.contains(target)
            this@Cache.selectedSolver = target
            return sequenceOf(request.replyWith(result))
        }
    }

    inner class DynamicCacheBranch : PrimitiveWithSignature {

        override val signature = Signature("context_branch", 2)

        override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
            val target: Int = request.arguments[0].castToInteger().intValue.toInt()
            val result: Term = request.arguments[1]
            this@Cache.dynamicSolver[this@Cache.nextSolver] =
                MutableSolver.classic.mutableSolverOf(Theory.empty(), MutableTheory.of(this@Cache.dynamicSolver[target]!!.dynamicKb))
                    .also { it.setFlag(Unknown.name, Unknown.FAIL) }
            this@Cache.selectedSolver = this@Cache.nextSolver++
            return sequenceOf(request.replyWith(Substitution.of(result.castToVar(), Numeric.of(this@Cache.selectedSolver))))
        }
    }

    inner class DynamicCacheAssert : PrimitiveWithSignature {

        override val signature = Signature("context_assert", 1)

        override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {

            val term: Term = request.arguments[0]
            this@Cache.dynamicSolver[this@Cache.selectedSolver]!!.assertA(term.castToStruct())
            return sequenceOf(request.replyWith(true))
        }
    }

    inner class DynamicCacheRetract : PrimitiveWithSignature {

        override val signature = Signature("context_retract", 1)

        override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
            val term: Term = request.arguments[0]
            this@Cache.dynamicSolver[this@Cache.selectedSolver]!!.retractAll(term.castToStruct())
            return sequenceOf(request.replyWith(true))
        }
    }

    inner class DynamicCacheGet : PrimitiveWithSignature {

        override val signature = Signature("context_check", 1)

        override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
            val term: Term = request.arguments[0]

            return sequence {
                yieldAll(
                    this@Cache.dynamicSolver[this@Cache.selectedSolver]!!.solve(term.castToStruct()).map {
                        request.replyWith(it.substitution)
                    }
                )
            }
        }
    }

    inner class DynamicCacheGetIndexed : PrimitiveWithSignature {

        override val signature = Signature("context_check", 2)

        override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
            val index: Int = request.arguments[0].asNumeric()!!.intValue.toInt()
            val term: Term = request.arguments[1]

            return sequence {
                yieldAll(
                    this@Cache.dynamicSolver[index]!!.solve(term.castToStruct()).map {
                        request.replyWith(it.substitution)
                    }
                )
            }
        }
    }

    inner class CacheAssert : PrimitiveWithSignature {

        override val signature = Signature("cache_assert", 1)

        override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {

            val term: Term = request.arguments[0]
            this@Cache.solver.assertA(term.castToStruct())
            return sequenceOf(request.replyWith(true))
        }
    }

    inner class CacheRetract : PrimitiveWithSignature {

        override val signature = Signature("cache_retract", 1)

        override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
            val term: Term = request.arguments[0]
            this@Cache.solver.retractAll(term.castToStruct())
            return sequenceOf(request.replyWith(true))
        }
    }

    inner class CacheGet : PrimitiveWithSignature {

        override val signature = Signature("cache_check", 1)

        override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
            val term: Term = request.arguments[0]

            return sequence {
                yieldAll(
                    this@Cache.solver.solve(term.castToStruct()).map {
                        request.replyWith(it.substitution)
                    }
                )
            }
        }
    }

    override val alias = "prolog.argumentation.cache"

    override val baseContent: AliasedLibrary
        get() =
            listOf(
                CacheAssert(),
                CacheRetract(),
                CacheGet(),
                DynamicCacheReset(),
                DynamicCacheCheckout(),
                DynamicCacheSelected(),
                DynamicCacheBranch(),
                DynamicCacheAssert(),
                DynamicCacheRetract(),
                DynamicCacheGet(),
                DynamicCacheGetIndexed()
            ).let { primitives ->
                Library.aliased(
                    alias = this.alias,
                    primitives = primitives.associateBy { it.signature }
                )
            }

    override val baseFlags: Iterable<ArgsFlag<*, *>>
        get() = emptyList()
}
