package it.unibo.tuprolog.argumentation.core.libraries.framework.context.helpers

import it.unibo.tuprolog.argumentation.engine.vm.ClauseStore
import it.unibo.tuprolog.argumentation.engine.vm.Program
import it.unibo.tuprolog.argumentation.engine.vm.Record
import it.unibo.tuprolog.argumentation.engine.vm.RecordIdentifiers
import it.unibo.tuprolog.argumentation.engine.vm.RecordKey
import it.unibo.tuprolog.core.Term

/**
 * The branches of a context: each a store of clauses (see [ClauseStore]), one of them active, a new one forked from
 * another; and the identifiers of the terms (context_id/2), shared by all the branches. Branch 0 is the root.
 *
 * The identifiers are a table of the context's own, or that of the session of its program (see SolverSession), shared
 * with the contexts of the other solvers of the session: the context takes it when bound to a program of the session
 * with no identifier given yet, or when reset (an identifier given before would be another one in it), and keeps it when
 * reset.
 */
internal class ContextBranches(
    /** What the library keeping its facts here tells of them */
    val layout: ContextLayout = ContextLayout.NONE,
) {
    private var next: Int = 1
    private var encoding: Program? = null
    private val stores: MutableMap<Int, ClauseStore> = mutableMapOf(0 to ClauseStore(declared = layout.indexed))

    /** The stores on the words of [program]: those there, and those made from now on */
    fun bind(program: Program) {
        if (encoding === program) return
        encoding = program
        stores.values.forEach { it.bind(program) }
        val shared = program.session?.identifiers
        if (identifiers === shared) return
        // the table of the program's session, or one of its own, when no identifier was given, or those given are the
        // session's and no store holds them (the context was reset)
        if (identifiers.size == 0 || (identifiers.concurrent && stores.values.all { it.isEmpty() })) {
            identifiers = shared ?: RecordIdentifiers(false)
            return
        }
        // the identifiers as they are, for the words of the program, in a table of the context's own
        val known = identifiers.all()
        identifiers = RecordIdentifiers(false)
        known.forEach { identifier(it) }
    }

    private var identifiers = RecordIdentifiers(false)

    /** The active branch */
    var active: Int = 0
        private set

    /** The store of the active branch */
    val selected: ClauseStore
        get() = stores[active]!!

    /** The branch the next one created will be */
    val nextBranch: Int
        get() = next

    /** The most recently created branch */
    val last: Int
        get() = next - 1

    /** The store of [branch]; null if there is no such branch (never created, or dropped) */
    fun storeOf(branch: Int): ClauseStore? = stores[branch]

    /** How many times the context was reset (see [reset]) */
    var resets = 0
        private set

    /** Back to a single empty branch, the root, and no identifiers (but those of the session, kept) */
    fun reset() {
        resets++
        active = 0
        next = 1
        stores.clear()
        stores[0] = ClauseStore(encoding, layout.indexed)
        val table = encoding?.session?.identifiers ?: RecordIdentifiers(false)
        identifiers = table
    }

    /** [branch] made active; false if there is no such branch */
    fun checkout(branch: Int): Boolean {
        if (branch !in stores) return false
        active = branch
        return true
    }

    /** A new branch forked from [from], made active; null if there is no branch [from] */
    fun branch(from: Int): Int? {
        val source = stores[from] ?: return null
        stores[next] = source.fork()
        active = next++
        return active
    }

    /** [branch] dropped; false for the active one */
    fun drop(branch: Int): Boolean {
        if (branch == active) return false
        stores.remove(branch)
        return true
    }

    /** The branches created after [after] dropped, but those in [keep] and the active one */
    fun dropAfter(
        after: Int,
        keep: Set<Int>,
    ) {
        stores.keys.removeAll { it > after && it !in keep && it != active }
    }

    private fun encoded(record: Record): Record = Record.encoded(record, encoding ?: Program().also { encoding = it })

    /**
     * The identifier of [record], shared by all the copies of the term which only differ in variable names (variables
     * get renamed whenever a term is read back from the context)
     */
    fun identifier(record: Record): Int = identifiers.identifier(encoded(record))

    fun identifier(term: Term): Int = identifier(Record.of(term))

    /** The identifier of the term with [key] in the words of [program], if it has one already and they are the ones here */
    internal fun knownIdentifier(
        key: RecordKey,
        program: Program?,
    ): Int? = if (encoding?.readsWordsOf(program) == true) identifiers.known(key) else null

    /** The identifier of [term] if it has one already (see [identifier]) */
    fun knownIdentifier(term: Term): Int? = identifiers.known(encoded(Record.of(term)).key)

    /** The record with identifier [identifier], as first given an identifier (in the words here); null if none has it */
    fun record(identifier: Int): Record? = identifiers.record(identifier)?.let { encoded(it) }

    /** The term with identifier [identifier], as first given an identifier; null if none has it */
    fun term(identifier: Int): Term? = record(identifier)?.term
}
