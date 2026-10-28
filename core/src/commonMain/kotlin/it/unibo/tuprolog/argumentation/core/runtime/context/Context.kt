package it.unibo.tuprolog.argumentation.core.runtime.context

import it.unibo.tuprolog.argumentation.core.flags.ArgsFlag
import it.unibo.tuprolog.argumentation.core.modules.ArgContext
import it.unibo.tuprolog.argumentation.core.modules.ArgLibrary
import it.unibo.tuprolog.argumentation.core.primitives.context.ContextAbsent
import it.unibo.tuprolog.argumentation.core.primitives.context.ContextAbsentIn
import it.unibo.tuprolog.argumentation.core.primitives.context.ContextActive
import it.unibo.tuprolog.argumentation.core.primitives.context.ContextAll
import it.unibo.tuprolog.argumentation.core.primitives.context.ContextAssert
import it.unibo.tuprolog.argumentation.core.primitives.context.ContextAssertAll
import it.unibo.tuprolog.argumentation.core.primitives.context.ContextBranch
import it.unibo.tuprolog.argumentation.core.primitives.context.ContextCheck
import it.unibo.tuprolog.argumentation.core.primitives.context.ContextCheckIn
import it.unibo.tuprolog.argumentation.core.primitives.context.ContextCheckout
import it.unibo.tuprolog.argumentation.core.primitives.context.ContextClosure
import it.unibo.tuprolog.argumentation.core.primitives.context.ContextCount
import it.unibo.tuprolog.argumentation.core.primitives.context.ContextDrop
import it.unibo.tuprolog.argumentation.core.primitives.context.ContextDropAfter
import it.unibo.tuprolog.argumentation.core.primitives.context.ContextId
import it.unibo.tuprolog.argumentation.core.primitives.context.ContextLast
import it.unibo.tuprolog.argumentation.core.primitives.context.ContextReset
import it.unibo.tuprolog.argumentation.core.primitives.context.ContextRetract
import it.unibo.tuprolog.argumentation.core.primitives.context.ContextUnifiable
import it.unibo.tuprolog.core.Clause
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.library.Library
import it.unibo.tuprolog.solve.library.Runtime

/**
 * The context of an arg2p solver: clauses kept in branches (see [ContextBranches]), outside of the theory and of
 * backtracking, which the context_* primitives change and read (see the files of the primitives of the context, on
 * branches, assertions and lookups).
 */
class Context :
    ArgLibrary,
    ArgContext {
    private val branches = ContextBranches()

    // Operations of the context for the native primitives of the other libraries, which reach it through [of]
    // rather than solving goals (a solver is set up to solve a goal)

    /** The active branch, as context_active/1 */
    val active: Int
        get() = branches.active

    /** A new branch of the given one, made active, as context_branch/2; null if there is no branch [from] */
    fun branch(from: Int): Int? = branches.branch(from)

    /**
     * A value derived from the clauses of a predicate in the given branch, the active one if null (see
     * [ClauseStore.derived]); for a branch that does not exist, computed on no clause
     */
    fun <T : Any> derived(
        functor: String,
        arity: Int,
        name: String,
        branch: Int? = null,
        compute: (kotlin.collections.List<Clause>) -> T,
    ): T {
        val store = if (branch == null) branches.selected else branches.storeOf(branch) ?: return compute(emptyList())
        return store.derived(functor, arity, name, compute)
    }

    /** The terms asserted in the active branch, in order, as context_assert_all/1 */
    fun assertAll(terms: Iterable<Term>) = terms.forEach { branches.selected.assertA(it) }

    /** The identifier of [term], as context_id/2 gives it */
    fun identifier(term: Term): Int = branches.identifier(term)

    /** The identifier of [term] if it has one already, without giving it one */
    fun knownIdentifier(term: Term): Int? = branches.knownIdentifier(term)

    /** The term with identifier [identifier] (see [identifier]), if any */
    fun term(identifier: Int): Term? = branches.term(identifier)

    /**
     * The instances of goal in the given branch (the active one if null), as context_all: for the native primitives
     * reading the context, without solving a goal (see [ContextAll.owner])
     */
    fun instances(
        goal: Term,
        branch: Int? = null,
    ): kotlin.collections.List<Term> {
        val store = if (branch == null) branches.selected else branches.storeOf(branch) ?: return emptyList()
        return store.solve(goal).map { goal.apply(it) }.toList()
    }

    override val alias = "prolog.argumentation.context"

    override val baseContent: Library
        get() =
            listOf(
                ContextReset(branches),
                ContextCheckout(branches),
                ContextActive(branches),
                ContextBranch(branches),
                ContextDrop(branches),
                ContextDropAfter(branches),
                ContextLast(branches),
                ContextAssert(branches),
                ContextAssertAll(branches),
                ContextRetract(branches),
                ContextCheck(branches),
                ContextCheckIn(branches),
                ContextAll(2, branches, this),
                ContextAll(3, branches, this),
                ContextId(branches),
                ContextUnifiable(branches),
                ContextAbsent(branches),
                ContextCount(branches),
                ContextAbsentIn(branches),
                ContextClosure(branches),
            ).let { primitives ->
                Library.of(
                    alias = this.alias,
                    primitives = primitives.associateBy { it.signature },
                )
            }

    override val baseFlags: Iterable<ArgsFlag<*, *>>
        get() = emptyList()

    companion object {
        /** The context library of the solver of the given context, if it is reachable (through context_all/2) */
        fun of(context: ExecutionContext): Context? = of(context.libraries)

        /** The context library among the given libraries, if any (through context_all/2) */
        fun of(libraries: Runtime): Context? = (libraries.primitives[CONTEXT_ALL] as? ContextAll)?.owner

        private val CONTEXT_ALL = Signature("context_all", 2)
    }
}
