package it.unibo.tuprolog.argumentation.core.modules.data

import it.unibo.tuprolog.argumentation.core.modules.PrologModule

/**
 * The term of a statement (module `statementTerm`, as `statement` is the statement labelling of that name): `[Claim]`, the list of the term claimed, as the conclusions of the
 * arguments and the statements labelled are written. Kotlin reads and builds it through [claim] and [of].
 */
object StatementTermModule : PrologModule("prolog.argumentation.data.statement", "statementTerm") {
    override val prologRawTheory: String =
        """
        %! new(?Claim, ?Statement)
        %  Statement is the statement of Claim.
        new(Claim, [Claim]).
        """.trimIndent()
}
