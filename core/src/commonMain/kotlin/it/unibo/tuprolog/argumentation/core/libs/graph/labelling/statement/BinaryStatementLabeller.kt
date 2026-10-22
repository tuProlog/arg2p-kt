package it.unibo.tuprolog.argumentation.core.libs.graph.labelling.statement

import it.unibo.tuprolog.argumentation.core.libs.PrologModule

/**
 * Two labels for the statements (module `statement_binary`, a statementLabellingMode): IN if an argument for the
 * statement is IN, OUT if one is OUT or UND.
 */
object BinaryStatementLabeller : PrologModule("prolog.argumentation.graph.labelling.statement.binary", "statement_binary") {
    override val prologRawTheory: String =
        """
        %! statementLabelling
        %  The labels of the statements (statIn/1 and statOut/1 in the context).
        statementLabelling :-
            findall(_, (
                argument::conclusion(A, Conc),
                context_check(in(A)),
                context_absent(statIn(Conc)),
                context_assert(statIn(Conc))
            ), _),
            findall(_, (
                argument::conclusion(A, Conc),
                (context_check(out(A)) ; context_check(und(A))),
                context_absent(statOut(Conc)),
                context_assert(statOut(Conc))
            ), _).
        """.trimIndent()
}
