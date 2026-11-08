package it.unibo.tuprolog.argumentation.parallel.store

import it.unibo.tuprolog.argumentation.core.engine.NativeCall
import it.unibo.tuprolog.argumentation.core.engine.Record
import it.unibo.tuprolog.argumentation.core.primitives.graph.GraphMark
import it.unibo.tuprolog.argumentation.core.primitives.graph.GraphSharing
import it.unibo.tuprolog.argumentation.core.primitives.graph.argumentConclusion
import it.unibo.tuprolog.argumentation.core.primitives.graph.contentKey
import it.unibo.tuprolog.argumentation.core.primitives.graph.graphKey
import it.unibo.tuprolog.argumentation.core.primitives.graph.graphMarkNow
import it.unibo.tuprolog.argumentation.core.primitives.graph.graphResets
import it.unibo.tuprolog.argumentation.core.primitives.graph.graphSegmentSince
import it.unibo.tuprolog.argumentation.core.primitives.graph.graphSession
import it.unibo.tuprolog.argumentation.core.primitives.graph.holdsFact
import it.unibo.tuprolog.argumentation.core.primitives.graph.mergeGraphSegment
import it.unibo.tuprolog.argumentation.core.primitives.graph.mergeSegmentTerms
import it.unibo.tuprolog.argumentation.core.primitives.graph.savedArgumentIds
import it.unibo.tuprolog.argumentation.core.primitives.graph.segmentTerms
import it.unibo.tuprolog.argumentation.core.primitives.graph.storedFacts
import it.unibo.tuprolog.argumentation.core.primitives.graph.termIn
import it.unibo.tuprolog.argumentation.parallel.host.locked
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.solve.Solver
import it.unibo.tuprolog.core.List as LogicList

/**
 * The graph the solvers of a session build together, each with its part of the rules, by the pipeline as it is
 * (structuredWhole::buildGraph, structuredPartial::queryGraph, pipeline::modifyGraph): none of them knows of
 * the others. The graph store calls this where what the others built matters (see GraphSharing): what a solver saves is
 * its segment, published when its argument builder asks what is new; the segments of the others are merged into its
 * store then, their conclusions new for its rules (see [interested]); and when it has nothing new, it waits for another
 * segment, or for every solver to have nothing new either, which ends the arguments. While the graph is built a solver
 * goes through its own arguments (the attacks on them it finds) and looks up those of every solver; buildGraph ends
 * once every solver has passed its graph on, every solver then having all of it. Then each solver changes the graph by
 * the steps of the pipeline, each step once every solver has merged what the others did before it: a step dividing the
 * graph goes through the arguments of the solver, and the attacks on them, the others through the whole graph, every
 * solver getting the same. The attacks a solver removes the others remove too.
 *
 * The solvers reach the state they share (a [SharedStore]) through a [StoreConnection] each: those of this process read
 * the segments of the others as they are; those of another process their terms (see SegmentTerms). A solver whose
 * context was reset once the graph is built (a solver solving another goal on it) builds it again by reading what every
 * solver built, no other involved. A solver failing ([fail]) fails those waiting, and those that wait later.
 *
 * Set as [it.unibo.tuprolog.argumentation.core.engine.vm.SolverSession.shared] of the session of the solvers in this
 * process, one for each graph: with a store of its own for [consumers] solvers ([SharedGraph]), each solver given a
 * connection to it when it first asks; or, for solvers of a graph of another process ([remote]), each given the connection
 * it reaches that graph by ([join]).
 */
class SharedGraph private constructor(
    /** In query mode: the solvers explore a statement (see explore), their argument builders share nothing */
    val exploring: Boolean,
    private val store: SharedStore?,
) : GraphSharing {
    /** A graph of [consumers] solvers of this process (and maybe of others, see [join]), its state here */
    constructor(consumers: Int, exploring: Boolean = false) : this(exploring, SharedStore(consumers)) {
        capacity = consumers
    }

    // the consumers of the graph, if known here: a solver asking past them would never let it end
    private var capacity = Int.MAX_VALUE

    /** Whether a solver building the graph failed (its store here told so): the graph is to be built anew */
    val failed: Boolean get() = store?.failed != null

    /** Whether the graph failed as a host running some of its consumers left: the goal building it is solved again */
    val lost: Boolean get() = store?.lost == true

    private val lock = Any()
    private val consumers = HashMap<Any, Consumer>()

    // the connection of the consumer 0, for a graph of another thread or process (see remote)
    private var connection0: StoreConnection? = null
    private val ids = HashSet<Int>()

    /** The state of the graph, kept here (null for a graph of another process): for the consumers of other processes */
    internal val state: SharedStore? get() = store

    /** The host running each consumer (for the statistics of an evaluation), as they were placed */
    var sites: List<String> = emptyList()

    /** The arguments of the consumers the statistics of an evaluation reported so far */
    var reported: Int = 0

    /** The arguments and the attacks each consumer published, by consumer (none for a graph of another process) */
    fun published(): Map<Int, Pair<Int, Int>> = store?.published().orEmpty()

    // (under the lock) an id no consumer has, for a solver of this process asking first
    private fun freeId(): Int {
        var id = 0
        while (id in ids) id++
        ids += id
        return id
    }

    private class Consumer(
        val connection: StoreConnection,
        // the resets of its context when it joined, or last read the graph from its start
        var resets: Int,
    ) {
        // where its own next segment starts, the arguments merged from others (not its own)
        var mark = GraphMark.START
        var merged = HashSet<Int>()

        // in query mode, the statements explored it passed on (an exploration repeated from scratch forgets them and
        // explores them again: they go by what they are, not where they are stored); the attacks it saved and those it
        // removed since it last passed them on (the steps changing the graph remove some, which moves the others in the
        // store: they go as they are saved, not where they are stored)
        var passedOn = HashSet<Any>()
        var saved = ArrayList<Record>()
        var removed = ArrayList<Record>()

        // the barriers it reached (see step), and whether the work is divided (the graph built, a step dividing it): its
        // enumerations of the store going through its own arguments
        var barriers = 0
        var dividing = true

        // the predicates (p/1, -p/1) of the conclusions of the others its rules build on (see interested), null for all
        var interest: Set<String>? = null

        // whether it reads the graph again (its context reset once it was built): what it saves the others built on
        // already, nothing passed on
        var again = false
    }

    // The consumer of the solver [key] (its context reset since: reading the graph from its start, as a new one)
    private fun consumer(
        key: Any,
        resets: Int,
    ): Consumer {
        val consumer =
            locked(lock) {
                consumers[key] ?: run {
                    val store = store ?: throw IllegalStateException("A solver not joined to the graph of another process")
                    if (ids.size >= capacity) {
                        val reason = "a solver not joined to the graph asked for it, past its $capacity solvers"
                        store.fail(reason)
                        throw IllegalStateException(reason)
                    }
                    Consumer(DirectConnection(store, freeId()), resets).also { consumers[key] = it }
                }
            }
        if (consumer.resets != resets) {
            // (a consumer joining before the reset its goal begins with is not one reading the graph again)
            val rereading = consumer.again || consumer.mark != GraphMark.START || consumer.barriers > 0
            consumer.again = rereading
            consumer.resets = resets
            consumer.mark = GraphMark.START
            consumer.merged = HashSet()
            consumer.passedOn = HashSet()
            consumer.saved = ArrayList()
            consumer.removed = ArrayList()
            consumer.barriers = 0
            consumer.dividing = true
            if (rereading) consumer.connection.rewind()
        }
        return consumer
    }

    private fun consumer(call: NativeCall): Consumer = consumer(call.graphKey(), call.graphResets())

    private fun known(call: NativeCall): Consumer? = locked(lock) { consumers[call.graphKey()] }?.takeIf { it.resets == call.graphResets() }

    /** A solver failed, for [reason]: those waiting fail, and those that would wait */
    fun fail(reason: String) {
        if (store != null) {
            store.fail(reason)
        } else {
            locked(lock) { consumers.values.firstOrNull() }?.connection?.fail(reason)
        }
    }

    /**
     * [solver], a solver of this process, joins the graph through [connection] (the graph of another process: see [remote]),
     * building with its rules on the conclusions of the others of [interest] alone (see [interested])
     */
    internal fun join(
        solver: Solver,
        connection: StoreConnection,
        interest: Set<String>?,
    ) {
        val key = solver.graphKey() ?: return
        val consumer = Consumer(connection, solver.graphResets())
        consumer.interest = interest
        locked(lock) { consumers[key] = consumer }
    }

    /**
     * [solver] joins the store of this graph through text, as a solver of another process does (see [LoopbackLine]): for
     * the tests of what goes between processes
     */
    internal fun throughText(
        solver: Solver,
        interest: Set<String>?,
    ) {
        val store = store!!
        store.remote = true
        join(solver, MessageConnection(LoopbackLine(store, locked(lock) { freeId() })), interest)
    }

    /**
     * [solver], of this process, is the consumer [id] of the graph (the ids of the consumers of other processes are taken by
     * them), building with its rules on the conclusions of the others of [interest] alone (see [interested])
     */
    fun assign(
        solver: Solver,
        id: Int,
        interest: Set<String>?,
    ) {
        assign(solver.graphKey() ?: return, solver.graphResets(), id, interest)
    }

    /** [assign] for the solver of [call] */
    fun assign(
        call: NativeCall,
        id: Int,
        interest: Set<String>?,
    ) = assign(call.graphKey(), call.graphResets(), id, interest)

    /** Whether the solver of [call] is a consumer of the graph */
    fun joined(call: NativeCall): Boolean = locked(lock) { call.graphKey() in consumers }

    private fun assign(
        key: Any,
        resets: Int,
        id: Int,
        interest: Set<String>?,
    ) {
        val consumer = Consumer(store?.let { DirectConnection(it, id) } ?: connection0!!, resets)
        consumer.interest = interest
        locked(lock) {
            ids += id
            consumers[key] = consumer
        }
    }

    /** The ids [taken] are those of consumers of other processes: what is published here goes with its terms */
    fun remoteConsumers(taken: Collection<Int>) {
        if (taken.isEmpty()) return
        locked(lock) { ids += taken }
        store?.remote = true
    }

    /**
     * The solver [solver] builds, with its rules, on the conclusions of the others of [predicates] alone (`p/1`,
     * `-p/1`), null for all of them: a predicate only the rules every solver has conclude from facts placed by their keys
     * each builds on its own (see the partitioner of the parallel solver). The arguments of the others are in its store all the
     * same, for every lookup: only what is new for its rules is.
     */
    fun interested(
        solver: Solver,
        predicates: Set<String>?,
    ) {
        val key = solver.graphKey() ?: return
        consumer(key, solver.graphResets()).interest = predicates
    }

    private fun checked(delivery: Delivery): Delivery {
        delivery.failure?.let { throw IllegalStateException("A solver building the graph failed: $it") }
        return delivery
    }

    private fun publish(
        call: NativeCall,
        consumer: Consumer,
    ) {
        val (graph, next) = call.graphSegmentSince(consumer.mark)
        consumer.mark = next
        if (consumer.again) {
            consumer.saved = ArrayList()
            consumer.removed = ArrayList()
            return
        }
        val explored = if (!exploring) emptyList() else call.storedFacts("explored", 1).filter { consumer.passedOn.add(it.contentKey) }
        val attacks = consumer.saved
        consumer.saved = ArrayList()
        val removed = consumer.removed
        consumer.removed = ArrayList()
        val segment = graph.with(attacks, explored, removed)
        if (segment.isEmpty) return
        val terms = if (consumer.connection.remote) call.segmentTerms(segment) else null
        consumer.connection.publish(Entry(-1, call.graphSession(), segment, terms))
    }

    // [taken] merged into the store of the solver, its conclusions new under [tag] if any: how many arguments are new for
    // its rules (with a tag), or here (without). What is new for the rules of the solver, under [tag]: an argument merged
    // its rules do not build on leaves the builder with nothing new, and it waits as one with nothing new
    private fun merge(
        call: NativeCall,
        consumer: Consumer,
        taken: List<Entry>,
        tag: String?,
        into: MutableList<Int>? = null,
    ): Int {
        var added = 0
        val interest = consumer.interest
        val noted: (Record) -> Boolean = { conclusion ->
            (interest == null || predicateOf(conclusion.term) in interest).also { if (it) added++ }
        }
        val session = call.graphSession()
        for (entry in taken) {
            val segment = entry.segment
            val ids =
                if (segment != null && entry.session === session) {
                    call.mergeGraphSegment(segment, tag, noted)
                } else {
                    call.mergeSegmentTerms(entry.terms!!, tag, noted)
                }
            ids.forEach { consumer.merged += it }
            into?.let { list -> ids.forEach { list += it } }
            if (tag == null) added += ids.size
        }
        // (the merged facts are not published again: the next segment starts after them)
        consumer.mark = call.graphMarkNow()
        return added
    }

    override fun share(
        call: NativeCall,
        tag: String,
        nothingNew: Boolean,
    ) {
        if (exploring) return
        val consumer = consumer(call)
        publish(call, consumer)
        if (merge(call, consumer, checked(consumer.connection.poll()).entries, tag) > 0 || !nothingNew) return
        while (true) {
            val delivery = checked(consumer.connection.await())
            if (delivery.over) return
            if (merge(call, consumer, delivery.entries, tag) > 0) return
        }
    }

    override fun explore(call: NativeCall): Pair<List<Term>, List<Term>>? {
        val consumer = consumer(call)
        publish(call, consumer)
        var taken = checked(consumer.connection.poll()).entries
        while (true) {
            val found = explorable(call, consumer, taken)
            if (found.first.isNotEmpty() || found.second.isNotEmpty()) return found
            val delivery = checked(consumer.connection.await())
            if (delivery.over) return null
            taken = delivery.entries
        }
    }

    // [taken] merged, and what it brings to explore: the statements explored by the others and not here, the claims of the
    // arguments new here
    private fun explorable(
        call: NativeCall,
        consumer: Consumer,
        taken: List<Entry>,
    ): Pair<List<Term>, List<Term>> {
        val statements = ArrayList<Term>()
        val claims = ArrayList<Term>()
        val session = call.graphSession()
        for (entry in taken) {
            val segment = entry.segment
            if (segment != null && entry.session === session) {
                // (read in the words of this program: the other's is another thread's)
                for (explored in segment.explored) {
                    if (!call.holdsFact(explored)) statements += call.termIn(explored).castToStruct()[0]
                }
            } else {
                for (statement in entry.terms!!.explored) {
                    if (!call.holdsFact(Record.of(Struct.of("explored", statement)))) statements += statement
                }
            }
            val ids =
                if (segment != null &&
                    entry.session === session
                ) {
                    call.mergeGraphSegment(segment, null)
                } else {
                    call.mergeSegmentTerms(entry.terms!!, null)
                }
            ids.forEach { consumer.merged += it }
            for (id in ids) (call.argumentConclusion(id).term as? LogicList)?.toList()?.firstOrNull()?.let { claims += it }
        }
        consumer.mark = call.graphMarkNow()
        return statements to claims
    }

    override fun settle(call: NativeCall) {
        val consumer = consumer(call)
        barrier(call, consumer)
        consumer.dividing = false
    }

    override fun step(
        call: NativeCall,
        ownAttacks: Boolean,
    ) {
        val consumer = consumer(call)
        barrier(call, consumer)
        consumer.dividing = ownAttacks
    }

    // The next barrier of the solver: once every solver reached it, what it had not read merged (the identifiers of the
    // arguments new here)
    private fun barrier(
        call: NativeCall,
        consumer: Consumer,
    ): IntArray {
        publish(call, consumer)
        val delivery = checked(consumer.connection.barrier(consumer.barriers++))
        val merged = ArrayList<Int>()
        merge(call, consumer, delivery.entries, null, merged)
        return merged.toIntArray()
    }

    override fun sync(call: NativeCall): IntArray = barrier(call, consumer(call))

    override fun removed(
        call: NativeCall,
        facts: List<Record>,
    ) {
        consumer(call).removed += facts
    }

    override fun saved(
        call: NativeCall,
        fact: Record,
    ) {
        consumer(call).saved += fact
    }

    override fun own(call: NativeCall): IntArray? {
        val consumer = known(call) ?: return null
        return if (consumer.dividing) ownOf(call, consumer) else null
    }

    private fun ownOf(
        call: NativeCall,
        consumer: Consumer,
    ): IntArray = call.savedArgumentIds().filter { it !in consumer.merged }.toIntArray()

    companion object {
        /** The graph of another process, for the solvers of this one that build it: each joins it by a connection of its own ([join]) */
        fun remote(exploring: Boolean): SharedGraph = SharedGraph(exploring, null)

        /** [remote], the consumer 0 of which, a solver of this process, reaches the graph through [connection] (see [assign]) */
        internal fun remote(
            exploring: Boolean,
            connection: StoreConnection,
        ): SharedGraph = SharedGraph(exploring, null).also { it.connection0 = connection }

        // The predicate of a conclusion, [Statement], as the interests name it
        internal fun predicateOf(conclusion: Term): String? {
            val statement = (conclusion as? LogicList)?.toList()?.firstOrNull() ?: return null
            if (statement is Struct && statement.functor == "-" && statement.arity == 1) {
                val inner = statement[0]
                return "-" + if (inner is Struct) "${inner.functor}/${inner.arity}" else inner.toString()
            }
            return if (statement is Struct) "${statement.functor}/${statement.arity}" else statement.toString()
        }
    }
}
