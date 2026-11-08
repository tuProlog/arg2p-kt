package it.unibo.tuprolog.argumentation.core.primitives.graph

import it.unibo.tuprolog.argumentation.InternalArg2pApi
import it.unibo.tuprolog.argumentation.core.engine.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.core.engine.NativeCall
import it.unibo.tuprolog.argumentation.core.engine.Record
import it.unibo.tuprolog.argumentation.core.engine.items
import it.unibo.tuprolog.argumentation.core.engine.replyWord
import it.unibo.tuprolog.argumentation.core.runtime.context.Context
import it.unibo.tuprolog.argumentation.core.runtime.context.contextLibrary
import it.unibo.tuprolog.core.Atom
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.Solver
import it.unibo.tuprolog.core.List as LogicList

/**
 * The graph a solver builds with others, through its graph store: the store calls it where what the others built
 * matters (see the module graphStore), and does nothing more than a store of its own where there is none. Set as
 * [it.unibo.tuprolog.argumentation.core.engine.vm.SolverSession.shared] of the session of the solvers: the module parallel
 * provides it (segments, barriers, the transports between the solvers); core only calls it.
 */
@InternalArg2pApi
interface GraphSharing {
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
private fun NativeCall.graphSharing(): GraphSharing? =
    contextLibrary
        .store()
        ?.encoding
        ?.session
        ?.shared as? GraphSharing

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
        .ids("arg")
        .toIntArray()

/** The facts of `name/arity` stored in the active branch, in the order they were */
@InternalArg2pApi
fun NativeCall.storedFacts(
    name: String,
    arity: Int,
): List<Record> = contextLibrary.store()!!.recordsFrom(name, arity, 0).first

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
    return argument.sub(conclusionWord(argument))
}

/** What [this] is as a key: the same for two records of one term up to the names of their variables */
@InternalArg2pApi
val Record.contentKey: Any get() = key

/** graphStore::graph_share(+Tag, +NothingNew): see [GraphSharing.share]; nothing outside a shared graph */
internal object GraphShare : AbstractNativePrimitive(Signature("graph_share", 2)) {
    override fun call(call: NativeCall): Int {
        val graph = call.graphSharing() ?: return NativeCall.CONTINUE
        graph.share(call, call.atomName(call.arg(0)), call.atomName(call.arg(1)) == "true")
        return NativeCall.CONTINUE
    }
}

/**
 * graphStore::graph_explore(-Statements, -Claims): in query mode, what the solver is to carry its exploration on with
 * (see [GraphSharing.explore]); fails outside a shared graph, and once the exploration is over
 */
internal object GraphExplore : AbstractNativePrimitive(Signature("graph_explore", 2)) {
    override fun call(call: NativeCall): Int {
        val (statements, claims) = call.graphSharing()?.explore(call) ?: return NativeCall.FAIL
        if (!call.unify(call.arg(0), call.word(LogicList.of(statements)))) return NativeCall.FAIL
        return call.replyWord(1, call.word(LogicList.of(claims)))
    }
}

/** graphStore::graph_begin(+Statement): see [GraphSharing.begin], Statement `whole` for the whole graph; nothing outside a shared graph */
internal object GraphBegin : AbstractNativePrimitive(Signature("graph_begin", 1)) {
    override fun call(call: NativeCall): Int {
        val graph = call.graphSharing() ?: return NativeCall.CONTINUE
        val statement = call.term(call.arg(0))
        graph.begin(call, statement.takeUnless { it is Atom && it.value == "whole" })
        return NativeCall.CONTINUE
    }
}

/** graphStore::graph_step(+Scope): see [GraphSharing.step], Scope `own` or `all`; nothing outside a shared graph */
internal object GraphStep : AbstractNativePrimitive(Signature("graph_step", 1)) {
    override fun call(call: NativeCall): Int {
        call.graphSharing()?.step(call, call.atomName(call.arg(0)) == "own")
        return NativeCall.CONTINUE
    }
}

/** graphStore::graph_removed(+Facts): the attacks removed, attackId/4 facts (see [GraphSharing.removed]); nothing outside a shared graph */
internal object GraphRemoved : AbstractNativePrimitive(Signature("graph_removed", 1)) {
    override fun call(call: NativeCall): Int {
        val graph = call.graphSharing() ?: return NativeCall.CONTINUE
        val facts = call.items(call.arg(0)) ?: return NativeCall.CONTINUE
        graph.removed(call, facts.map { call.record(call.deref(it)) })
        return NativeCall.CONTINUE
    }
}

/** graphStore::graph_attack_saved(+Fact): the attack saved, an attackId/4 fact (see [GraphSharing.saved]); nothing outside a shared graph */
internal object GraphAttackSaved : AbstractNativePrimitive(Signature("graph_attack_saved", 1)) {
    override fun call(call: NativeCall): Int {
        call.graphSharing()?.saved(call, call.record(call.arg(0)))
        return NativeCall.CONTINUE
    }
}

/** graphStore::graph_settle: see [GraphSharing.settle]; nothing outside a shared graph */
internal object GraphSettle : AbstractNativePrimitive(Signature("graph_settle", 0)) {
    override fun call(call: NativeCall): Int {
        call.graphSharing()?.settle(call)
        return NativeCall.CONTINUE
    }
}

/** graphStore::graph_own(-Ids): the arguments the solver saved itself, while the work on a shared graph is divided (see [GraphSharing.own]); fails otherwise */
internal object GraphOwn : AbstractNativePrimitive(Signature("graph_own", 1)) {
    override fun call(call: NativeCall): Int {
        val own = call.graphSharing()?.own(call) ?: return NativeCall.FAIL
        return call.replyWord(0, call.list(IntArray(own.size) { call.int(own[it]) }))
    }
}

/** graphStore::graph_sync(-Ids): see [GraphSharing.sync]; none outside a shared graph */
internal object GraphSync : AbstractNativePrimitive(Signature("graph_sync", 1)) {
    override fun call(call: NativeCall): Int {
        val merged = call.graphSharing()?.sync(call) ?: IntArray(0)
        return call.replyWord(0, call.list(IntArray(merged.size) { call.int(merged[it]) }))
    }
}
