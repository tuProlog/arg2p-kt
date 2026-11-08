package it.unibo.tuprolog.argumentation.core.primitives.graph

import it.unibo.tuprolog.argumentation.core.runtime.graph.graphSharing
import it.unibo.tuprolog.argumentation.engine.vm.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.engine.vm.NativeCall
import it.unibo.tuprolog.argumentation.engine.vm.items
import it.unibo.tuprolog.argumentation.engine.vm.replyWord
import it.unibo.tuprolog.core.Atom
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.core.List as LogicList

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
