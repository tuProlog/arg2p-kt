package it.unibo.tuprolog.argumentation.core.engine.fsm

import it.unibo.tuprolog.core.Clause
import it.unibo.tuprolog.core.Rule
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.extractSignature
import it.unibo.tuprolog.theory.Theory
import it.unibo.tuprolog.unify.Unificator
import it.unibo.tuprolog.utils.Cursor

/**
 * Clause selection (arg2p): the clauses of an immutable theory are grouped by the signature of their heads once, and
 * those whose heads match a goal are collected in a list. Theories index their
 * clauses in RETE trees, whose lookups merge lazy sequences (of each index) at every step: for the few clauses of
 * a predicate, the scan is cheaper.
 */
internal class ClauseIndex private constructor(
    theory: Theory,
) {
    private val bySignature: Map<Signature, List<Clause>> =
        theory.clauses
            .filter { it.head != null }
            .groupBy { it.head!!.extractSignature() }

    fun collect(
        goal: Struct,
        unificator: Unificator,
        into: MutableList<Clause>,
    ) {
        bySignature[goal.extractSignature()]?.forEach { clause ->
            if (unificator.match(goal, clause.head!!)) into.add(clause)
        }
    }

    companion object {
        // The indexes of the last theories met, by identity: the libraries of each module, and the static theory
        private const val CACHED = 64
        private val theories = arrayOfNulls<Theory>(CACHED)
        private val indexes = arrayOfNulls<ClauseIndex>(CACHED)
        private var last = 0

        fun of(theory: Theory): ClauseIndex {
            for (i in 0 until CACHED) {
                if (theories[i] === theory) return indexes[i]!!
            }
            val index = ClauseIndex(theory)
            last = (last + 1) % CACHED
            theories[last] = theory
            indexes[last] = index
            return index
        }
    }
}

/**
 * The clauses selected for a goal, from the one at [index], as stored: their variables are renamed as they are
 * resolved (see [ClauseResolution])
 */
internal class RuleCursor private constructor(
    private val clauses: List<Clause>,
    private val index: Int,
) : Cursor<Rule> {
    override val current: Rule? by lazy(LazyThreadSafetyMode.NONE) {
        if (index < clauses.size) clauses[index].castToRule() else null
    }

    override val next: Cursor<out Rule> by lazy(LazyThreadSafetyMode.NONE) {
        if (index + 1 < clauses.size) RuleCursor(clauses, index + 1) else Cursor.empty()
    }

    // As for the cursors of tuProlog, whether there is a current clause
    override val hasNext: Boolean
        get() = index < clauses.size

    override val isOver: Boolean
        get() = index >= clauses.size

    override val isLazy: Boolean
        get() = true

    override fun <R> map(mapper: (Rule) -> R): Cursor<out R> = Cursor.of(asSequence().map(mapper))

    override fun iterator(): Iterator<Rule> =
        object : Iterator<Rule> {
            private var cursor: Cursor<out Rule> = this@RuleCursor

            override fun hasNext(): Boolean = !cursor.isOver

            override fun next(): Rule = cursor.current!!.also { cursor = cursor.next }
        }

    override fun toString(): String = if (isOver) "[]" else "[${clauses[index]}, ...]"

    companion object {
        fun of(clauses: List<Clause>): Cursor<out Rule> = if (clauses.isEmpty()) Cursor.empty() else RuleCursor(clauses, 0)
    }
}
