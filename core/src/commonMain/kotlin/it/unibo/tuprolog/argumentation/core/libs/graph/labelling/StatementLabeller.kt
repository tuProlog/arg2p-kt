package it.unibo.tuprolog.argumentation.core.libs.graph.labelling

import it.unibo.tuprolog.argumentation.core.libs.PrologModule

object StatementLabeller : PrologModule("prolog.argumentation.graph.labelling.statement", "statement") {
    override val prologRawTheory: String =
        """
        statementLabelling :-
            findall(_, (
                context_check(in([_, _, Conc, _, _])),
                context_absent(statIn(Conc)),
                context_assert(statIn(Conc))
            ), _),
            findall(_, (
                context_check(out([_, _, Conc, _, _])),
                context_absent(statIn(Conc)),
                context_absent(statOut(Conc)),
                context_assert(statOut(Conc))
            ), _),
            findall(_, (
                context_check(und([_, _, Conc, _, _])),
                context_absent(statIn(Conc)),
                context_absent(statOut(Conc)),
                context_absent(statUnd(Conc)),
                context_assert(statUnd(Conc))
            ), _).
        """.trimIndent()
}
