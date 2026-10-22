package it.unibo.tuprolog.argumentation.core.libs.data

import it.unibo.tuprolog.argumentation.core.libs.PrologModule
import it.unibo.tuprolog.core.List
import it.unibo.tuprolog.core.Term

/**
 * The term of a statement (module `statementTerm`, as `statement` is the statement labelling of that name): `[Claim]`, the list of the term claimed, as the conclusions of the
 * arguments and the statements labelled are written. Kotlin reads and builds it through [claim] and [of].
 */
object StatementTerm : PrologModule("prolog.argumentation.data.statement", "statementTerm") {
    override val prologRawTheory: String =
        """
        %! new(?Claim, ?Statement)
        %  Statement is the statement of Claim.
        new(Claim, [Claim]).
        """.trimIndent()

    /** The term [statement] claims; null if it is not a statement */
    fun claim(statement: Term): Term? = (statement as? List)?.toList()?.singleOrNull()

    /** The statement of [claim] */
    fun of(claim: Term): Term = List.of(claim)
}
