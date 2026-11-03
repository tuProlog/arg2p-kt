package it.unibo.tuprolog.argumentation.core.primitives.graph

import it.unibo.tuprolog.argumentation.core.engine.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.core.engine.NativeCall
import it.unibo.tuprolog.argumentation.core.engine.answersOf
import it.unibo.tuprolog.argumentation.core.engine.termArg
import it.unibo.tuprolog.argumentation.core.runtime.renamedCopy
import it.unibo.tuprolog.argumentation.core.runtime.settings.Settings
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.solve.Signature

/**
 * theoryClause(?Head, ?Body): the clauses of the theory, Head :- Body (Body true for a fact), each a fresh copy, but
 * the facts stating flags: read as data, as clause/2 reads them, for the heads it cannot read as well (those of the
 * control constructs, such as ->/2, which the notations of some profiles use)
 */
internal object TheoryClause : AbstractNativePrimitive(Signature("theoryClause", 2)) {
    override fun call(call: NativeCall): Int {
        val context = call.context
        val settings = Settings.of(context)
        val wanted = Struct.of("clause", call.termArg(0), call.termArg(1))
        val substitutions =
            context.staticKb.clauses
                .asSequence()
                .filter { it.head != null && settings?.states(it) != true }
                .mapNotNull { clause ->
                    val copy = clause.renamedCopy()
                    context.unificator.mgu(wanted, Struct.of("clause", copy.head!!, copy.body)).takeIf { it.isSuccess }
                }
        return call.answersOf(substitutions.iterator())
    }
}
