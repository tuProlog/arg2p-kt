package it.unibo.tuprolog.argumentation.core.modules.labelling.statement

import it.unibo.tuprolog.argumentation.core.modules.PrologModule

/**
 * The labels of the statements passed through from those of their arguments (module `statement_pass_through`, a
 * statementLabellingMode): a statement takes the label of each of its arguments, so it may have several.
 */
object StatementPassThroughModule : PrologModule("prolog.argumentation.graph.labelling.statement.pt", "statement_pass_through") {
    override val prologRawTheory: String =
        // language=Prolog
        """
        %! statementLabelling
        %  The labels of the statements (see graphStore::labelStatement/2), one for each argument.
        statementLabelling :-
            findall(_, (
                argument::conclusion(A, Conc),
                graphStore::argumentLabel(Label, A),
                graphStore::labelStatement(Label, Conc)
            ), _).
        """.trimIndent()
}
