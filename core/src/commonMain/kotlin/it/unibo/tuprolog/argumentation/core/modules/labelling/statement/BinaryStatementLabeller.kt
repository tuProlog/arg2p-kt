package it.unibo.tuprolog.argumentation.core.modules.labelling.statement

import it.unibo.tuprolog.argumentation.core.libs.PrologModule

/**
 * Two labels for the statements (module `statement_binary`, a statementLabellingMode): IN if an argument for the
 * statement is IN, OUT if one is OUT or UND.
 */
object BinaryStatementLabeller : PrologModule("prolog.argumentation.graph.labelling.statement.binary", "statement_binary") {
    override val prologRawTheory: String =
        """
        %! statementLabelling
        %  The labels of the statements (see graphStore::labelStatement/2): IN those with an argument IN, OUT those with
        %  an argument OUT or UND.
        statementLabelling :-
            findall(_, (
                argument::conclusion(A, Conc),
                graphStore::argumentLabel(in, A),
                \+ graphStore::statementLabel(in, Conc),
                graphStore::labelStatement(in, Conc)
            ), _),
            findall(_, (
                argument::conclusion(A, Conc),
                (graphStore::argumentLabel(out, A) ; graphStore::argumentLabel(und, A)),
                \+ graphStore::statementLabel(out, Conc),
                graphStore::labelStatement(out, Conc)
            ), _).
        """.trimIndent()
}
