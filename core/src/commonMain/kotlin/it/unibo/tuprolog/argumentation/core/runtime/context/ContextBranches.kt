package it.unibo.tuprolog.argumentation.core.runtime.context

import it.unibo.tuprolog.argumentation.core.engine.Record
import it.unibo.tuprolog.argumentation.core.engine.RecordKey
import it.unibo.tuprolog.argumentation.core.engine.vm.Program
import it.unibo.tuprolog.core.Term

/**
 * The branches of a context: each a store of clauses (see [ClauseStore]), one of them active, a new one forked from
 * another; and the identifiers of the terms (context_id/2), shared by all the branches. Branch 0 is the root.
 */
internal class ContextBranches {
    private var next: Int = 1
    private var encoding: Program? = null
    private val stores: MutableMap<Int, ClauseStore> = mutableMapOf(0 to ClauseStore())

    // The converted theory of the solver, kept as a template every solve starts from (context_template_save/0,
    // context_template_restore/0): the conversion is a function of the theory and of the flags, so the template holds
    // for the program it was made in (rebuilt when the flags or the knowledge bases change through the solver) and for
    // the version of its theory. Only a conversion starting from an empty branch is kept, and only an empty branch is
    // replaced by it: what a branch holds is never lost
    private var template: ClauseStore? = null
    private var templateVersion: Long? = null
    private var templateIdentifiers: Map<RecordKey, Int> = emptyMap()
    private var templateRecords: List<Record> = emptyList()
    private var convertingEmpty = false

    /** The stores on the words of [program]: those there, and those made from now on */
    fun bind(program: Program) {
        if (encoding === program) return
        template = null
        encoding = program
        stores.values.forEach { it.bind(program) }
        // the identifiers as they are, for the words of the program
        val known = records.toList()
        records.clear()
        identifiers.clear()
        known.forEach { identifier(it) }
    }

    /** The active branch kept as the template, if the conversion started from an empty one and [version] is known */
    fun saveTemplate(version: Long?) {
        if (!convertingEmpty || version == null) return
        template = selected.fork()
        templateVersion = version
        templateIdentifiers = HashMap(identifiers)
        templateRecords = ArrayList(records)
    }

    /**
     * The active branch, empty, replaced by a copy of the template of [version], the identifiers as they were saved;
     * false if it is not empty or there is no such template: the conversion is to be run
     */
    fun restoreTemplate(version: Long?): Boolean {
        convertingEmpty = selected.isEmpty()
        val template = template ?: return false
        if (!convertingEmpty || version == null || version != templateVersion) return false
        stores[active] = template.fork()
        identifiers.clear()
        identifiers.putAll(templateIdentifiers)
        records.clear()
        records.addAll(templateRecords)
        return true
    }

    private val identifiers = HashMap<RecordKey, Int>()
    private val records = ArrayList<Record>()

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

    /** Back to a single empty branch, the root, and no identifiers */
    fun reset() {
        active = 0
        next = 1
        stores.clear()
        stores[0] = ClauseStore(encoding)
        identifiers.clear()
        records.clear()
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
    fun identifier(record: Record): Int {
        val encoded = encoded(record)
        return identifiers.getOrPut(encoded.key) {
            encoded.identifier = records.size
            records += encoded
            records.size - 1
        }
    }

    fun identifier(term: Term): Int = identifier(Record.of(term))

    /** The identifier of [term] if it has one already (see [identifier]) */
    fun knownIdentifier(term: Term): Int? = identifiers[encoded(Record.of(term)).key]

    /** The record with identifier [identifier], as first given an identifier; null if none has it */
    fun record(identifier: Int): Record? = records.getOrNull(identifier)

    /** The term with identifier [identifier], as first given an identifier; null if none has it */
    fun term(identifier: Int): Term? = records.getOrNull(identifier)?.term
}
