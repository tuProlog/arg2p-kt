package it.unibo.tuprolog.argumentation.core.libraries.framework.store.primitives

import it.unibo.tuprolog.argumentation.core.libraries.framework.store.helpers.sharing.graphSharing
import it.unibo.tuprolog.argumentation.engine.natives.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.engine.natives.NativeCall
import it.unibo.tuprolog.argumentation.engine.natives.items
import it.unibo.tuprolog.argumentation.engine.natives.replyWord
import it.unibo.tuprolog.core.Atom
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.core.List as LogicList

/** store::graphShare(+Tag, +NothingNew): see [GraphSharing.share]; nothing outside a shared graph */
internal object GraphShare : AbstractNativePrimitive(Signature("graphShare", 2)) {
    override fun call(call: NativeCall): Int {
        val graph = call.graphSharing() ?: return NativeCall.CONTINUE
        graph.share(call, call.atomName(call.arg(0)), call.atomName(call.arg(1)) == "true")
        return NativeCall.CONTINUE
    }
}

/**
 * store::graphExplore(-Statements, -Claims): in query mode, what the solver is to carry its exploration on with
 * (see [GraphSharing.explore]); fails outside a shared graph, and once the exploration is over
 */
internal object GraphExplore : AbstractNativePrimitive(Signature("graphExplore", 2)) {
    override fun call(call: NativeCall): Int {
        val (statements, claims) = call.graphSharing()?.explore(call) ?: return NativeCall.FAIL
        if (!call.unify(call.arg(0), call.word(LogicList.of(statements)))) return NativeCall.FAIL
        return call.replyWord(1, call.word(LogicList.of(claims)))
    }
}

/** store::graphBegin(+Statement): see [GraphSharing.begin], Statement `whole` for the whole graph; nothing outside a shared graph */
internal object GraphBegin : AbstractNativePrimitive(Signature("graphBegin", 1)) {
    override fun call(call: NativeCall): Int {
        val graph = call.graphSharing() ?: return NativeCall.CONTINUE
        val statement = call.term(call.arg(0))
        graph.begin(call, statement.takeUnless { it is Atom && it.value == "whole" })
        return NativeCall.CONTINUE
    }
}

/** store::graphStep(+Scope): see [GraphSharing.step], Scope `own` or `all`; nothing outside a shared graph */
internal object GraphStep : AbstractNativePrimitive(Signature("graphStep", 1)) {
    override fun call(call: NativeCall): Int {
        call.graphSharing()?.step(call, call.atomName(call.arg(0)) == "own")
        return NativeCall.CONTINUE
    }
}

/** store::graphRemoved(+Facts): the attacks removed, attackId/4 facts (see [GraphSharing.removed]); nothing outside a shared graph */
internal object GraphRemoved : AbstractNativePrimitive(Signature("graphRemoved", 1)) {
    override fun call(call: NativeCall): Int {
        val graph = call.graphSharing() ?: return NativeCall.CONTINUE
        val facts = call.items(call.arg(0)) ?: return NativeCall.CONTINUE
        graph.removed(call, facts.map { call.record(call.deref(it)) })
        return NativeCall.CONTINUE
    }
}

/**
 * store::graphAttackSaved(+Type, +IdAttacker, +IdAttacked, +IdOn): the attack saved (see [GraphSharing.saved]), its
 * attackId/4 fact made only for a shared graph; nothing outside one
 */
internal object GraphAttackSaved : AbstractNativePrimitive(Signature("graphAttackSaved", 4)) {
    override fun call(call: NativeCall): Int {
        val graph = call.graphSharing() ?: return NativeCall.CONTINUE
        val fact = Struct.of("attackId", List(4) { call.term(call.arg(it)) })
        graph.saved(call, call.record(call.word(fact)))
        return NativeCall.CONTINUE
    }
}

/** store::graphSettle: see [GraphSharing.settle]; nothing outside a shared graph */
internal object GraphSettle : AbstractNativePrimitive(Signature("graphSettle", 0)) {
    override fun call(call: NativeCall): Int {
        call.graphSharing()?.settle(call)
        return NativeCall.CONTINUE
    }
}

/** store::graphOwn(-Ids): the arguments the solver saved itself, while the work on a shared graph is divided (see [GraphSharing.own]); fails otherwise */
internal object GraphOwn : AbstractNativePrimitive(Signature("graphOwn", 1)) {
    override fun call(call: NativeCall): Int {
        val own = call.graphSharing()?.own(call) ?: return NativeCall.FAIL
        return call.replyWord(0, call.list(IntArray(own.size) { call.int(own[it]) }))
    }
}

/** store::graphSync(-Ids): see [GraphSharing.sync]; none outside a shared graph */
internal object GraphSync : AbstractNativePrimitive(Signature("graphSync", 1)) {
    override fun call(call: NativeCall): Int {
        val merged = call.graphSharing()?.sync(call) ?: IntArray(0)
        return call.replyWord(0, call.list(IntArray(merged.size) { call.int(merged[it]) }))
    }
}
