package it.unibo.tuprolog.argumentation.core.model.terms

import it.unibo.tuprolog.core.List
import it.unibo.tuprolog.core.Term

/**
 * The term of a statement, `[Claim]`: the list of the term claimed, as the conclusions of the arguments and the
 * statements labelled are written
 */
object StatementTerm {
    /** The statement of [claim] */
    fun of(claim: Term): Term = List.of(claim)
}
