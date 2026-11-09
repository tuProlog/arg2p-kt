package it.unibo.tuprolog.argumentation.core.libraries.language.plainProlog.helpers

import it.unibo.tuprolog.argumentation.core.libraries.support.utils.helpers.signatureOf
import it.unibo.tuprolog.core.Clause
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.dsl.logicProgramming
import it.unibo.tuprolog.solve.ExecutionContext

/** A Prolog clause as a rule or a premise */
internal object ClauseConversion {
    /** The premises of the rule [target] stands for: `\+ G` a contrary, a Prolog goal a check, the others statements */
    fun modifiers(
        target: Clause,
        context: ExecutionContext,
    ): Term =
        logicProgramming {
            target.bodyItems
                .map { term ->
                    if (term.isStruct && term.asStruct()?.functor == "\\+") {
                        "~"(term.asStruct()!!.args[0])
                    } else if (isPrologGoal(term, context)) {
                        "prolog"(term)
                    } else {
                        term
                    }
                }.let { if (it.count() > 1) tupleOf(it) else it.first() }
        }

    /**
     * Whether a goal of a rule is a Prolog goal: one clause/2 cannot inspect (a variable, a term that is not
     * callable, or a procedure the libraries protect), rather than a statement of the theory
     */
    private fun isPrologGoal(
        goal: Term,
        context: ExecutionContext,
    ): Boolean = goal !is Struct || context.libraries.hasProtected(goal.signatureOf)

    /** The identifier of the rule a Prolog clause stands for: from its position in the theory, the same at each conversion */
    fun ruleId(index: Int): String = "rule_$index"

    // The functors of the facts written in the language of arg2p (rules, premises, axioms), not Prolog facts
    private val arg2pFacts = setOf(":->", "->", "=>", ":=>", ":", ",")

    /** Whether [clause] is a Prolog fact, rather than a rule, a premise or an axiom written in the language of arg2p */
    fun isPrologFact(clause: Clause): Boolean = clause.isFact && clause.head!!.functor !in arg2pFacts
}
