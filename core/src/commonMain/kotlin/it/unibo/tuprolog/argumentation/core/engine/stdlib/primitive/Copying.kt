package it.unibo.tuprolog.argumentation.core.engine.stdlib.primitive

import it.unibo.tuprolog.argumentation.core.runtime.renamedCopy
import it.unibo.tuprolog.core.Substitution
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.primitive.BinaryRelation
import it.unibo.tuprolog.solve.primitive.Solve
import it.unibo.tuprolog.solve.stdlib.primitive.AbstractCollectingPrimitive
import it.unibo.tuprolog.core.List as LogicList

/**
 * copy_term/2 of 2p-kt, copying its term by the identity of its variables (see [renamedCopy]): 2p-kt copies them by
 * name, joining the variables of the renamings of a clause and parting the occurrences of an anonymous one.
 */
object CopyTerm : BinaryRelation.Functional<ExecutionContext>("copy_term") {
    override fun Solve.Request<ExecutionContext>.computeOneSubstitution(
        first: Term,
        second: Term,
    ): Substitution = mgu(first.renamedCopy(), second)
}

/** findall/3 of 2p-kt, copying each solution by the identity of its variables (see [CopyTerm]) */
object FindAll : AbstractCollectingPrimitive("findall") {
    override fun Solve.Request<ExecutionContext>.computeAllSubstitutions(
        first: Term,
        second: Term,
        third: Term,
    ): Sequence<Substitution> {
        ensuringArgumentIsInstantiated(1)
        ensuringArgumentIsCallable(1)
        val solutions = computeIntermediateSolutions(second.castToStruct())
        val mapped = solutions.map { first[it.substitution].renamedCopy() }
        return sequenceOf(mgu(third, LogicList.of(mapped)))
    }
}
