package it.unibo.tuprolog.argumentation.core.libs.graph.labelling

import it.unibo.tuprolog.argumentation.core.libs.PrologModule

/**
 * The labels of the statements from those of their arguments (module `statement`, a statementLabellingMode): a
 * statement is IN if an argument for it is IN, otherwise OUT if one is OUT, otherwise UND if one is UND.
 */
object StatementLabeller : PrologModule("prolog.argumentation.graph.labelling.statement", "statement") {
    override val prologRawTheory: String =
        """
        %! statementLabelling
        %  The labels of the statements (statIn/1, statOut/1, statUnd/1 in the context).
        statementLabelling :-
            findall(_, (
                argument::conclusion(A, Conc),
                context_check(in(A)),
                context_absent(statIn(Conc)),
                context_assert(statIn(Conc))
            ), _),
            findall(_, (
                argument::conclusion(A, Conc),
                context_check(out(A)),
                context_absent(statIn(Conc)),
                context_absent(statOut(Conc)),
                context_assert(statOut(Conc))
            ), _),
            findall(_, (
                argument::conclusion(A, Conc),
                context_check(und(A)),
                context_absent(statIn(Conc)),
                context_absent(statOut(Conc)),
                context_absent(statUnd(Conc)),
                context_assert(statUnd(Conc))
            ), _).
        """.trimIndent()
}
