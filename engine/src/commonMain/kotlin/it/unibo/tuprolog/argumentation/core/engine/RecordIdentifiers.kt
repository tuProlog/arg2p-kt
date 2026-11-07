package it.unibo.tuprolog.argumentation.core.engine

import it.unibo.tuprolog.argumentation.core.engine.vm.Symbols
import it.unibo.tuprolog.argumentation.core.engine.vm.locked

/**
 * The identifiers of records (see `context_id/2`): a record has the identifier of the first record with its key, the
 * same for two terms up to the names of their variables, and the identifiers are given in order, from 0. A context has
 * a table of its own, or shares that of its session ([concurrent]: given under a lock, a record read by its identifier
 * without one, as an identifier reaches another thread after it was given).
 */
internal class RecordIdentifiers(
    val concurrent: Boolean,
) {
    private val identifiers = HashMap<RecordKey, Int>()
    private val records = Symbols.Table<Record>()

    /** How many records have an identifier */
    val size: Int get() = records.size

    /** The identifier of [encoded], given if it has none (the record kept as the one of its identifier) */
    fun identifier(encoded: Record): Int {
        val key = encoded.key
        return changing {
            identifiers.getOrPut(key) {
                encoded.identifier = records.size
                records.add(encoded)
            }
        }
    }

    /** The identifier of the records with [key], if given */
    fun known(key: RecordKey): Int? = changing { identifiers[key] }

    /** The record with [identifier], as first given it; null if none has it */
    fun record(identifier: Int): Record? = if (identifier >= 0 && identifier < records.size) records[identifier] else null

    /** The records, by identifier */
    fun all(): List<Record> = List(records.size) { records[it] }

    /** A table of its own with the same identifiers */
    fun copy(): RecordIdentifiers {
        check(!concurrent) { "The identifiers of a session are not copied" }
        return RecordIdentifiers(false).also { copy ->
            copy.identifiers.putAll(identifiers)
            copy.records.addAll(records)
        }
    }

    private inline fun <T> changing(crossinline change: () -> T): T = if (concurrent) locked(this) { change() } else change()
}
