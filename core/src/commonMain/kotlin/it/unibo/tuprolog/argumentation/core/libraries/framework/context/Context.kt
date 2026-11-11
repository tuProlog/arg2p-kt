package it.unibo.tuprolog.argumentation.core.libraries.framework.context

import it.unibo.tuprolog.argumentation.EarlierSpellings
import it.unibo.tuprolog.argumentation.core.libraries.ArgLibrary
import it.unibo.tuprolog.argumentation.core.libraries.framework.context.helpers.ContextBranches
import it.unibo.tuprolog.argumentation.core.libraries.framework.context.helpers.ContextLayout
import it.unibo.tuprolog.argumentation.core.libraries.framework.context.primitives.ContextAbsent
import it.unibo.tuprolog.argumentation.core.libraries.framework.context.primitives.ContextActive
import it.unibo.tuprolog.argumentation.core.libraries.framework.context.primitives.ContextAssert
import it.unibo.tuprolog.argumentation.core.libraries.framework.context.primitives.ContextAssertId
import it.unibo.tuprolog.argumentation.core.libraries.framework.context.primitives.ContextBranch
import it.unibo.tuprolog.argumentation.core.libraries.framework.context.primitives.ContextBy
import it.unibo.tuprolog.argumentation.core.libraries.framework.context.primitives.ContextCheck
import it.unibo.tuprolog.argumentation.core.libraries.framework.context.primitives.ContextCheckIn
import it.unibo.tuprolog.argumentation.core.libraries.framework.context.primitives.ContextCheckout
import it.unibo.tuprolog.argumentation.core.libraries.framework.context.primitives.ContextCount
import it.unibo.tuprolog.argumentation.core.libraries.framework.context.primitives.ContextDrop
import it.unibo.tuprolog.argumentation.core.libraries.framework.context.primitives.ContextDropAfter
import it.unibo.tuprolog.argumentation.core.libraries.framework.context.primitives.ContextId
import it.unibo.tuprolog.argumentation.core.libraries.framework.context.primitives.ContextLast
import it.unibo.tuprolog.argumentation.core.libraries.framework.context.primitives.ContextMatching
import it.unibo.tuprolog.argumentation.core.libraries.framework.context.primitives.ContextReset
import it.unibo.tuprolog.argumentation.core.libraries.framework.context.primitives.ContextRetract
import it.unibo.tuprolog.argumentation.core.libraries.framework.context.primitives.ContextRetractAll
import it.unibo.tuprolog.argumentation.core.libraries.framework.context.primitives.ContextStoredId
import it.unibo.tuprolog.argumentation.core.libraries.framework.context.primitives.ContextTerm
import it.unibo.tuprolog.argumentation.core.libraries.framework.loader.helpers.OwnedLibrary
import it.unibo.tuprolog.argumentation.engine.store.ClauseStore
import it.unibo.tuprolog.argumentation.engine.store.ProgramStore
import it.unibo.tuprolog.argumentation.engine.store.Record
import it.unibo.tuprolog.argumentation.engine.vm.program.Program
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.solve.library.Library
import it.unibo.tuprolog.solve.library.Runtime

/**
 * The context of an arg2p solver: clauses kept in branches (see [ContextBranches]), outside of the theory and of
 * backtracking, which the context_* primitives change and read (see the files of the primitives of the context, on
 * branches, assertions and lookups).
 */
internal class Context(
    private val onReset: (Long?) -> Unit = {},
    layout: ContextLayout = ContextLayout.NONE,
) : ArgLibrary,
    ProgramStore {
    private val branches = ContextBranches(layout)

    // Operations of the context for the native primitives of the other libraries, which reach it through [of]
    // rather than solving goals (a solver is set up to solve a goal)

    /** The active branch, as contextActive/1 */
    val active: Int
        get() = branches.active

    /** A new branch of the given one, made active, as contextBranch/2; null if there is no branch [from] */
    fun branch(from: Int): Int? = branches.branch(from)

    /**
     * A value derived from the clauses of a predicate in the given branch, the active one if null (see
     * [ClauseStore.derived]); for a branch that does not exist, computed on no clause
     */
    fun <T : Any> derived(
        key: String,
        name: String,
        branch: Int? = null,
        compute: (kotlin.collections.List<ClauseStore.Stored>) -> T,
    ): T {
        val store = if (branch == null) branches.selected else branches.storeOf(branch) ?: return compute(emptyList())
        return store.derived(key, name, compute)
    }

    /** The store of the given branch, the active one if null; null for a branch that does not exist */
    fun store(branch: Int? = null): ClauseStore? = if (branch == null) branches.selected else branches.storeOf(branch)

    /** How many times the context was reset: a graph built in it before is gone */
    val resets: Int get() = branches.resets

    /** The identifier of [term], given one if it has none */
    internal fun identifier(term: Term): Int = branches.identifier(term)

    /** The identifier of [record] (see [identifier]) */
    internal fun identifier(record: Record): Int = branches.identifier(record)

    /** The identifier of [term] if it has one already, without giving it one */
    fun knownIdentifier(term: Term): Int? = branches.knownIdentifier(term)

    /** The record with identifier [identifier], if any */
    fun record(identifier: Int): Record? = branches.record(identifier)

    /** The term with identifier [identifier] (see [identifier]), if any */
    fun term(identifier: Int): Term? = branches.term(identifier)

    /** The stores on the words of [program] (the machine's) */
    override fun bind(program: Program) = branches.bind(program)

    override val alias = ALIAS

    override val baseContent: Library
        get() =
            listOf(
                ContextReset(branches, onReset),
                ContextCheckout(branches),
                ContextActive(branches),
                ContextBranch(branches),
                ContextDrop(branches),
                ContextDropAfter(branches),
                ContextLast(branches),
                ContextAssert(branches),
                ContextAssertId(branches),
                ContextRetract(branches),
                ContextRetractAll(branches),
                ContextCheck(branches),
                ContextCheckIn(branches),
                ContextId(branches),
                ContextTerm(branches),
                ContextBy(branches),
                ContextStoredId(branches),
                ContextMatching(branches),
                ContextAbsent(branches),
                ContextCount(branches),
            ).let { primitives ->
                OwnedLibrary(
                    this,
                    Library.of(
                        alias = this.alias,
                        primitives = EarlierSpellings.withEarlierNames(primitives.associateBy { it.signature }),
                    ),
                )
            }

    companion object {
        /** The context library among the given libraries, if any */
        fun of(libraries: Runtime): Context? {
            for (library in libraries.libraries) {
                if (library.alias == ALIAS) return (library as? OwnedLibrary)?.owner as? Context
            }
            return null
        }

        private const val ALIAS = "prolog.argumentation.context"
    }
}
