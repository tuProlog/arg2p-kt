package it.unibo.tuprolog.argumentation.core.modules.labelling.statement

import it.unibo.tuprolog.argumentation.core.modules.PrologModule

/**
 * Two labels for the statements (module `statement_binary`, a statementLabellingMode): IN if an argument for the
 * statement is IN, OUT otherwise.
 */
object StatementBinaryModule : PrologModule("prolog.argumentation.graph.labelling.statement.binary", "statement_binary") {
    override val prologRawTheory: String =
        // language=Prolog
        """
        %! statementLabelling
        %  The labels of the statements (see graphStore::labelStatement/2): IN those with an argument IN, OUT the others
        %  (their arguments are OUT or UND), each statement as it is written: one with variables and its instances are
        %  labelled each on its own (see graphStore::statementLabelled/2).
        statementLabelling :-
            findall(_, (
                argument::conclusion(A, Conc),
                graphStore::argumentLabel(in, A),
                \+ graphStore::statementLabelled(in, Conc),
                graphStore::labelStatement(in, Conc)
            ), _),
            findall(_, (
                argument::conclusion(A, Conc),
                (graphStore::argumentLabel(out, A) ; graphStore::argumentLabel(und, A)),
                \+ graphStore::statementLabelled(in, Conc),
                \+ graphStore::statementLabelled(out, Conc),
                graphStore::labelStatement(out, Conc)
            ), _).
        """.trimIndent()
}
