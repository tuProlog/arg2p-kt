package it.unibo.tuprolog.argumentation.core.libs.graph.labelling.statement

import it.unibo.tuprolog.argumentation.core.libs.PrologModule

/**
 * The labels of the statements passed through from those of their arguments (module `statement_pass_through`, a
 * statementLabellingMode): a statement takes the label of each of its arguments, so it may have several.
 */
object PassThroughStatementLabeller : PrologModule("prolog.argumentation.graph.labelling.statement.pt", "statement_pass_through") {
    override val prologRawTheory: String =
        """
        %! statementLabelling
        %  The labels of the statements (statIn/1, statOut/1, statUnd/1 in the context), one for each argument.
        statementLabelling :-
            findall(_, (argument::conclusion(A, Conc), context_check(in(A)), context_assert(statIn(Conc))), _),
            findall(_, (argument::conclusion(A, Conc), context_check(out(A)), context_assert(statOut(Conc))), _),
            findall(_, (argument::conclusion(A, Conc), context_check(und(A)), context_assert(statUnd(Conc))), _).
        """.trimIndent()
}
