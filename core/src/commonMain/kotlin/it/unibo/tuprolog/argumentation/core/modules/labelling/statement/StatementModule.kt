package it.unibo.tuprolog.argumentation.core.modules.labelling.statement

import it.unibo.tuprolog.argumentation.core.modules.PrologModule

/**
 * The labels of the statements from those of their arguments (module `statement`, a statementLabellingMode): a
 * statement is IN if an argument for it is IN, otherwise OUT if one is OUT, otherwise UND if one is UND.
 */
object StatementModule : PrologModule("prolog.argumentation.graph.labelling.statement", "statement") {
    override val prologRawTheory: String =
        """
        %! statementLabelling
        %  The labels of the statements (see graphStore::labelStatement/2): IN those with an argument IN, then OUT those
        %  with an argument OUT, then UND those with an argument UND.
        statementLabelling :-
            findall(_, (
                argument::conclusion(A, Conc),
                graphStore::argumentLabel(in, A),
                \+ graphStore::statementLabel(in, Conc),
                graphStore::labelStatement(in, Conc)
            ), _),
            findall(_, (
                argument::conclusion(A, Conc),
                graphStore::argumentLabel(out, A),
                \+ graphStore::statementLabel(in, Conc),
                \+ graphStore::statementLabel(out, Conc),
                graphStore::labelStatement(out, Conc)
            ), _),
            findall(_, (
                argument::conclusion(A, Conc),
                graphStore::argumentLabel(und, A),
                \+ graphStore::statementLabel(in, Conc),
                \+ graphStore::statementLabel(out, Conc),
                \+ graphStore::statementLabel(und, Conc),
                graphStore::labelStatement(und, Conc)
            ), _).
        """.trimIndent()
}
