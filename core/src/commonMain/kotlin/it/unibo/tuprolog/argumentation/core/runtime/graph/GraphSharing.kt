package it.unibo.tuprolog.argumentation.core.runtime.graph

import it.unibo.tuprolog.argumentation.InternalArg2pApi
import it.unibo.tuprolog.argumentation.core.runtime.context.Context
import it.unibo.tuprolog.argumentation.core.runtime.context.ContextSchema
import it.unibo.tuprolog.argumentation.core.runtime.context.contextLibrary
import it.unibo.tuprolog.argumentation.engine.vm.NativeCall
import it.unibo.tuprolog.argumentation.engine.vm.Record
import it.unibo.tuprolog.argumentation.engine.vm.SessionKey
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.solve.Solver

/**
 * The graph a solver builds with others, through its graph store: the store calls it where what the others built
 * matters (see the module graphStore), and does nothing more than a store of its own where there is none. Kept under
 * [KEY] in the session of the solvers: the module parallel provides it (segments, barriers, the transports between the
 * solvers); core only calls it.
 */
@InternalArg2pApi
interface GraphSharing {
    companion object {
        /** Where a session of solvers keeps its graph */
        val KEY = SessionKey<GraphSharing>()
    }

    /**
     * A graph begins to be built: the whole one ([statement] null), or in query mode the one of [statement]. The
     * solver a client asked chooses then the graph it builds with others (starting them if it is new); nothing for the
     * others.
     */
    fun begin(
        call: NativeCall,
        statement: Term?,
    ) = Unit

    /**
     * What the argument builder is new about, under [tag] (graphStore::newConclusion/2): what the solver saved passed
     * on, what the others did taken; with [nothingNew], until another has something or none can
     */
    fun share(
        call: NativeCall,
        tag: String,
        nothingNew: Boolean,
    )

    /**
     * In query mode, what the solver is to carry its exploration on with: the statements the others explored and the
     * conclusions of their arguments; null once the exploration is over
     */
    fun explore(call: NativeCall): Pair<List<Term>, List<Term>>?

    /** The graph of the solver passed on, and that of every solver taken, once all of them have built theirs (the end of buildGraph) */
    fun settle(call: NativeCall)

    /** Before a step changing the graph, or after the last ([ownAttacks] false): the same, the step dividing the attacks if [ownAttacks] */
    fun step(
        call: NativeCall,
        ownAttacks: Boolean,
    )

    /**
     * The arguments the solver saved itself, while the work is divided (the graph being built, a step dividing it): what
     * an enumeration of the store goes through then, a lookup going through every argument; null otherwise
     */
    fun own(call: NativeCall): IntArray?

    /**
     * Within a step adding arguments, before their attacks are found: once every solver reached this point, the
     * arguments the others added in the step merged, their identifiers given
     */
    fun sync(call: NativeCall): IntArray

    /** [facts], attacks the solver removed, for the others to remove too */
    fun removed(
        call: NativeCall,
        facts: List<Record>,
    )

    /** [fact], an attack the solver saved, for the others to save too */
    fun saved(
        call: NativeCall,
        fact: Record,
    )
}

// The graph the solver of [this] shares, if any
internal fun NativeCall.graphSharing(): GraphSharing? =
    contextLibrary
        .store()
        ?.encoding
        ?.session
        ?.get(GraphSharing.KEY)

// ---- what an implementation of GraphSharing reads of the store of a solver

/** The solver of the call, as one of those sharing a graph: its context */
@InternalArg2pApi
fun NativeCall.graphKey(): Any = contextLibrary

/** The solver, as [graphKey] knows it; null for a solver with no context */
@InternalArg2pApi
fun Solver.graphKey(): Any? = Context.of(libraries)

/** How many times the context of the solver of the call was reset: a graph built in it before is gone */
@InternalArg2pApi
fun NativeCall.graphResets(): Int = contextLibrary.resets

/** [graphResets] for [this] */
@InternalArg2pApi
fun Solver.graphResets(): Int = Context.of(libraries)?.resets ?: 0

/** The identifiers of the arguments saved in the active branch */
@InternalArg2pApi
fun NativeCall.savedArgumentIds(): IntArray =
    contextLibrary
        .store()!!
        .ids(ContextSchema.SAVED)
        .toIntArray()

/** The statements explored in the active branch, as the facts stating them, in the order they were */
@InternalArg2pApi
fun NativeCall.exploredFacts(): List<Record> =
    contextLibrary
        .store()!!
        .recordsFrom(ContextSchema.EXPLORED, ContextSchema.EXPLORED_ARITY, 0)
        .first

/** The fact stating that [statement] is explored */
@InternalArg2pApi
fun exploredFact(statement: Term): Record = Record.of(Struct.of(ContextSchema.EXPLORED, statement))

/** Whether the fact [record] is stored in the active branch, up to the names of its variables */
@InternalArg2pApi
fun NativeCall.holdsFact(record: Record): Boolean = contextLibrary.store()!!.holds(record)

/** The term of [record], read in the words of the solver of the call (a record of another solver is another thread's) */
@InternalArg2pApi
fun NativeCall.termIn(record: Record): Term = Record.encoded(record, contextLibrary.store()!!.encoding!!).term

/** The conclusion of the argument with identifier [id], [Statement], as a record in the words of the solver */
@InternalArg2pApi
fun NativeCall.argumentConclusion(id: Int): Record {
    val argument = contextLibrary.record(id)!!
    return argument.sub(ContextSchema.conclusionWord(argument))
}

/** What [this] is as a key: the same for two records of one term up to the names of their variables */
@InternalArg2pApi
val Record.contentKey: Any get() = key
