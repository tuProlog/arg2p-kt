package it.unibo.tuprolog.argumentation.core.engine.fsm

import it.unibo.tuprolog.argumentation.core.engine.ClauseSource
import it.unibo.tuprolog.argumentation.core.engine.PrologExecutionContext
import it.unibo.tuprolog.core.Clause
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.extractSignature
import it.unibo.tuprolog.theory.Theory
import it.unibo.tuprolog.unify.Unificator

/**
 * The clauses of the program of a resolution, from the libraries, the static and the dynamic theory, in this order
 * (arg2p).
 */
internal class ProgramClauses private constructor(
    private val sources: List<ClauseSource>,
) : ClauseSource {
    override fun clausesFor(
        goal: Struct,
        unificator: Unificator,
    ): List<Clause> {
        val clauses = ArrayList<Clause>()
        sources.forEach { clauses.addAll(it.clausesFor(goal, unificator)) }
        return clauses
    }

    companion object {
        /** The clauses of the program of [context] */
        fun of(context: PrologExecutionContext): ProgramClauses =
            with(context) {
                ProgramClauses(listOf(theory(libraries.asTheory(unificator)), theory(staticKb), theory(dynamicKb)))
            }

        private fun theory(theory: Theory): ClauseSource =
            if (theory.isMutable) MutableTheoryClauses(theory) else IndexedTheoryClauses.of(theory)
    }
}

/** The clauses of a mutable theory, as it is when looked up */
internal class MutableTheoryClauses(
    private val theory: Theory,
) : ClauseSource {
    override fun clausesFor(
        goal: Struct,
        unificator: Unificator,
    ): List<Clause> = theory[goal].toList()
}

/**
 * The clauses of an immutable theory, grouped by the signature of their heads once: those whose heads match a goal
 * are collected in a list. Theories index their clauses in RETE trees, whose lookups merge lazy sequences (of each
 * index) at every step: for the few clauses of a predicate, the scan is cheaper.
 */
internal class IndexedTheoryClauses private constructor(
    theory: Theory,
) : ClauseSource {
    private val bySignature: Map<Signature, List<Clause>> =
        theory.clauses
            .filter { it.head != null }
            .groupBy { it.head!!.extractSignature() }

    override fun clausesFor(
        goal: Struct,
        unificator: Unificator,
    ): List<Clause> = bySignature[goal.extractSignature()]?.filter { unificator.match(goal, it.head!!) } ?: emptyList()

    companion object {
        // The indexes of the last theories met, by identity: the libraries of each module, and the static theory
        private const val CACHED = 64
        private val theories = arrayOfNulls<Theory>(CACHED)
        private val indexes = arrayOfNulls<IndexedTheoryClauses>(CACHED)
        private var last = 0

        fun of(theory: Theory): IndexedTheoryClauses {
            for (i in 0 until CACHED) {
                if (theories[i] === theory) return indexes[i]!!
            }
            val index = IndexedTheoryClauses(theory)
            last = (last + 1) % CACHED
            theories[last] = theory
            indexes[last] = index
            return index
        }
    }
}
