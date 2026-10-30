package it.unibo.tuprolog.argumentation.core.engine.fsm

import it.unibo.tuprolog.core.Clause
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.Var
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.library.Runtime
import it.unibo.tuprolog.theory.Theory
import it.unibo.tuprolog.unify.Unificator

/**
 * The clauses of an immutable theory, indexed once (arg2p): by the functor and the arity of their heads, then by the
 * first argument of their heads (see [FirstArgumentIndex]); those matching a goal are collected in a list. Theories
 * index their clauses in RETE trees, whose lookups merge lazy sequences (of each index) at every step.
 */
internal class IndexedTheoryClauses(
    theory: Theory,
) : TheoryClauseSource {
    // By functor, then by arity: looked up without building the signature (or the indicator) of the goal
    private val byFunctor: Map<String, Map<Int, FirstArgumentIndex>> =
        theory.clauses
            .filter { it.head != null }
            .groupBy { it.head!!.functor }
            .mapValues { (_, clauses) ->
                clauses.groupBy { it.head!!.arity }.mapValues { (_, sameArity) -> FirstArgumentIndex(sameArity) }
            }

    // A single candidate is left to the resolution of the goal with it, which unifies them anyway: matching it first
    // would unify them twice. Among several, those not matching are left out, so that none is left to backtrack to.
    override fun clausesFor(
        goal: Struct,
        unificator: Unificator,
    ): List<Clause> {
        val candidates = byFunctor[goal.functor]?.get(goal.arity)?.candidates(goal) ?: return emptyList()
        return if (candidates.size == 1) candidates else candidates.filter { unificator.match(goal, it.head!!) }
    }

    override fun defines(goal: Struct): Boolean = byFunctor[goal.functor]?.containsKey(goal.arity) == true
}

/**
 * The indexes of the immutable theories a solver resolves with (arg2p): the libraries of each module and the static
 * theory, the last ones met, by identity. A solver keeps them for all its resolutions, and shares them with the
 * solvers its resolutions create (for sub-goals, modules), which run in the same thread: the indexes need no
 * synchronization, and the resolutions of solvers in other threads have indexes of their own.
 */
class TheoryIndexes {
    // Allocated when first needed: the root contexts of sub-solvers get indexes of their own before sharing others
    private val theories by lazy(LazyThreadSafetyMode.NONE) { arrayOfNulls<Theory>(CACHED) }
    private val indexes by lazy(LazyThreadSafetyMode.NONE) { arrayOfNulls<IndexedTheoryClauses>(CACHED) }
    private var last = 0

    /** The index of [theory], built the first time it is met */
    internal fun of(theory: Theory): IndexedTheoryClauses {
        for (i in 0 until CACHED) {
            if (theories[i] === theory) return indexes[i]!!
        }
        val index = IndexedTheoryClauses(theory)
        last = (last + 1) % CACHED
        theories[last] = theory
        indexes[last] = index
        return index
    }

    // The clauses of the programs met last (see [program]): libraries, static and dynamic theory, by identity
    private val programs = ArrayDeque<Pair<Triple<Any, Any, Any>, ProgramClauses>>()

    /**
     * The clauses of the program made of [libraries], [staticKb] and [dynamicKb], built by [build] the first time
     * they are met together (the libraries change with the modules called, the theories with the solvers)
     */
    internal fun program(
        libraries: Runtime,
        staticKb: Theory,
        dynamicKb: Theory,
        build: () -> ProgramClauses,
    ): ProgramClauses {
        for ((key, program) in programs) {
            if (key.first === libraries && key.second === staticKb && key.third === dynamicKb) return program
        }
        return build().also {
            if (programs.size == PROGRAMS) programs.removeLast()
            programs.addFirst(Triple(libraries, staticKb, dynamicKb) to it)
        }
    }

    private companion object {
        const val CACHED = 64
        const val PROGRAMS = 16
    }
}

/**
 * The clauses of a predicate indexed by the principal symbol of the first argument of their heads (a constant, or
 * the functor and arity of a compound term): the clauses for a goal whose first argument is bound are those with
 * the same symbol and those whose first argument is a variable, in their order.
 */
internal class FirstArgumentIndex(
    private val clauses: List<Clause>,
) {
    // The clauses whose first argument is a variable, which any goal may match
    private val open = ArrayList<Clause>()

    // For each symbol, the clauses with that symbol or a variable as first argument, in order
    private val bySymbol = HashMap<Any, MutableList<Clause>>()

    init {
        for (clause in clauses) {
            val symbol = clause.head!!.firstArgumentSymbol()
            if (symbol == null) {
                open.add(clause)
                bySymbol.values.forEach { it.add(clause) }
            } else {
                bySymbol.getOrPut(symbol) { ArrayList(open) }.add(clause)
            }
        }
    }

    /** The clauses whose heads may unify with [goal], in their order */
    fun candidates(goal: Struct): List<Clause> {
        val symbol = goal.firstArgumentSymbol() ?: return clauses
        return bySymbol[symbol] ?: open
    }

    private companion object {
        // The principal symbol of the first argument of a head, null if there is none or it is a variable
        fun Struct.firstArgumentSymbol(): Any? = if (arity == 0) null else symbol(this[0])

        fun symbol(term: Term): Any? =
            when {
                term is Var -> null
                term is Struct && term.arity > 0 -> Signature(term.functor, term.arity)
                else -> term
            }
    }
}
