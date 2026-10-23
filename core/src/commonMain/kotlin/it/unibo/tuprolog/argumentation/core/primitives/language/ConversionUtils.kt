package it.unibo.tuprolog.argumentation.core.primitives.language

import it.unibo.tuprolog.argumentation.core.flags.PrologStrictCompatibility
import it.unibo.tuprolog.argumentation.core.runtime.settings.Settings
import it.unibo.tuprolog.core.Clause
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Substitution
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.dsl.LogicProgrammingScope
import it.unibo.tuprolog.dsl.logicProgramming
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.extractSignature
import it.unibo.tuprolog.solve.primitive.PrimitiveWrapper.Companion.ensuringArgumentIsVariable
import it.unibo.tuprolog.solve.primitive.Solve

object ConversionUtils {
    fun modifiers(
        target: Clause,
        context: Solve.Request<ExecutionContext>,
    ): Term =
        logicProgramming {
            target.bodyItems
                .map { term ->
                    if (term.isStruct && term.asStruct()?.functor == "\\+") {
                        "~"(term.asStruct()!!.args[0])
                    } else if (isPrologGoal(term, context.context)) {
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
    ): Boolean = goal !is Struct || context.libraries.hasProtected(goal.extractSignature())

    /** The identifier of the rule a Prolog clause stands for: from its position in the theory, the same at each conversion */
    fun ruleId(index: Int): String = "rule_$index"

    fun commonMap(
        context: Solve.Request<ExecutionContext>,
        first: Term,
        force: Boolean = false,
        mapper: (Iterable<IndexedValue<Clause>>, LogicProgrammingScope) -> Term,
    ): Sequence<Substitution> {
        context.ensuringArgumentIsVariable(0)
        return logicProgramming {
            sequenceOf(
                Substitution.of(
                    first.castToVar(),
                    if (force || Settings.of(context.context)?.isOn(PrologStrictCompatibility, context.context) == true) {
                        mapper(
                            context.context.staticKb.clauses
                                .withIndex(),
                            this,
                        )
                    } else {
                        emptyLogicList
                    },
                ),
            )
        }
    }
}
