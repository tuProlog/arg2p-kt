package it.unibo.tuprolog.argumentation.core.modules.graph

import it.unibo.tuprolog.argumentation.core.modules.PrologModule
import it.unibo.tuprolog.argumentation.core.primitives.graph.TheoryClause
import it.unibo.tuprolog.argumentation.core.runtime.settings.Profiles
import it.unibo.tuprolog.core.operators.OperatorSet
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.primitive.Primitive

/**
 * A module reading the notation of the abstract profile (`a -> b.`, `a <-> b.`): its clauses written with the operators
 * of the notation, and theoryClause/2, which reads the clauses of the theory as data (->/2 cannot be called)
 */
internal abstract class AbstractNotationModule(
    alias: String,
    identifier: String,
) : PrologModule(alias, identifier) {
    override val theoryOperators: OperatorSet
        get() = super.theoryOperators + Profiles.ABSTRACT.operators

    override val primitives: Map<Signature, Primitive>
        get() = mapOf(TheoryClause.signature to TheoryClause)
}
