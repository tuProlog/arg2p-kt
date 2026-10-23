package it.unibo.tuprolog.argumentation.core.primitives.language

import it.unibo.tuprolog.core.List
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Substitution
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.primitive.BinaryRelation
import it.unibo.tuprolog.solve.primitive.Solve

object ExtractId : BinaryRelation.WithoutSideEffects<ExecutionContext>("extract_id") {
    override fun Solve.Request<ExecutionContext>.computeAllSubstitutions(
        first: Term,
        second: Term,
    ): Sequence<Substitution> =
        sequenceOf(
            Substitution.of(
                second.asVar()!!,
                List.of(
                    first.asList()!!.toList().map {
                        Struct.of("strict", it.asCons()!!.head)
                    },
                ),
            ),
        )
}
