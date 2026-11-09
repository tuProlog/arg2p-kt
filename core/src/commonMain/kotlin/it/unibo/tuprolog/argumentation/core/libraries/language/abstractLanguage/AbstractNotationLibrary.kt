package it.unibo.tuprolog.argumentation.core.libraries.language.abstractLanguage

import it.unibo.tuprolog.argumentation.core.libraries.PrologLibrary
import it.unibo.tuprolog.argumentation.core.libraries.language.abstractLanguage.primitives.TheoryClause
import it.unibo.tuprolog.core.operators.OperatorSet
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.primitive.Primitive

/**
 * A module reading the notation of the abstract profile (`a -> b.`, `a <-> b.`): its clauses written with the operators
 * of the notation, and theoryClause/2, which reads the clauses of the theory as data (->/2 cannot be called)
 */
internal abstract class AbstractNotationLibrary(
    identifier: String,
) : PrologLibrary(identifier) {
    override val theoryOperators: OperatorSet
        get() = super.theoryOperators + AbstractNotation.operators

    override val notation: OperatorSet
        get() = AbstractNotation.operators

    override val primitives: Map<Signature, Primitive>
        get() = mapOf(TheoryClause.signature to TheoryClause)
}
