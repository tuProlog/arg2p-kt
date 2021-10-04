package it.unibo.tuprolog.argumentation.core.libs.core

import it.unibo.tuprolog.argumentation.core.libs.ArgsFlag
import it.unibo.tuprolog.argumentation.core.libs.BaseArgLibrary
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.MutableSolver
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.library.AliasedLibrary
import it.unibo.tuprolog.solve.library.Library
import it.unibo.tuprolog.solve.primitive.Primitive
import it.unibo.tuprolog.solve.primitive.Solve
import it.unibo.tuprolog.theory.MutableTheory
import it.unibo.tuprolog.theory.Theory

class Cache : BaseArgLibrary() {

    private val solver : MutableSolver = MutableSolver.classic.mutableSolverOf(Theory.empty(), MutableTheory.empty())

    inner class CacheAssert : Primitive {

        val signature = Signature("cache_assert", 1)

        override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
            val term: Term = request.arguments[0]
            this@Cache.solver.assertA(term.castToStruct())
            return sequenceOf(request.replyWith(true))
        }
    }

    inner class CacheRetract : Primitive {

        val signature = Signature("cache_retract", 1)

        override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
            val term: Term = request.arguments[0]
            this@Cache.solver.retractAll(term.castToStruct())
            return sequenceOf(request.replyWith(true))
        }
    }

    inner class CacheGet : Primitive {

        val signature = Signature("cache_check", 1)

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
        get() {
            val assert = CacheAssert()
            val retract = CacheRetract()
            val get = CacheGet()
            return Library.aliased(
                alias = this.alias,
                primitives = mapOf(
                    assert.signature to assert,
                    retract.signature to retract,
                    get.signature to get
                )
            )
        }

    override val baseFlags: Iterable<ArgsFlag<*, *>>
        get() = emptyList()
}
