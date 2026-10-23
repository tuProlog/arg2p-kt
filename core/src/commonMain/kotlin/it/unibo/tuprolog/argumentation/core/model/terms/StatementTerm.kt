package it.unibo.tuprolog.argumentation.core.model.terms

import it.unibo.tuprolog.argumentation.core.modules.data.StatementTermModule
import it.unibo.tuprolog.core.List
import it.unibo.tuprolog.core.Term

/** The term of the module StatementTermModule as Kotlin reads and builds it (see [StatementTermModule]) */
object StatementTerm {
    /** The term [statement] claims; null if it is not a statement */
    fun claim(statement: Term): Term? = (statement as? List)?.toList()?.singleOrNull()

    /** The statement of [claim] */
    fun of(claim: Term): Term = List.of(claim)
}
