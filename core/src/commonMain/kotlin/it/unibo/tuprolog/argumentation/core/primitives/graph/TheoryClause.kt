package it.unibo.tuprolog.argumentation.core.primitives.graph

import it.unibo.tuprolog.argumentation.core.primitives.PrimitiveWithSignature
import it.unibo.tuprolog.argumentation.core.runtime.settings.Settings
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.primitive.Solve

/**
 * theoryClause(?Head, ?Body): the clauses of the theory, Head :- Body (Body true for a fact), each a fresh copy, but
 * the facts stating flags: read as data, as clause/2 reads them, for the heads it cannot read as well (those of the
 * control constructs, such as ->/2, which the notations of some profiles use)
 */
internal object TheoryClause : PrimitiveWithSignature {
    override val signature = Signature("theoryClause", 2)

    override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
        val settings = Settings.of(request.context)
        val wanted = Struct.of("clause", request.arguments)
        return request.context.staticKb.clauses
            .asSequence()
            .filter { it.head != null && settings?.states(it) != true }
            .mapNotNull { clause ->
                val copy = clause.freshCopy()
                request.context.unificator
                    .mgu(wanted, Struct.of("clause", copy.head!!, copy.body))
                    .takeIf { it.isSuccess }
                    ?.let { request.replyWith(it) }
            }
    }
}
