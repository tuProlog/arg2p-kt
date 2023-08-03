package it.unibo.tuprolog.argumentation.core.libs.graph.labelling

import it.unibo.tuprolog.argumentation.core.libs.ArgLibrary
import it.unibo.tuprolog.argumentation.core.libs.ArgsFlag
import it.unibo.tuprolog.argumentation.core.libs.Loadable
import it.unibo.tuprolog.argumentation.core.libs.basic.DynamicLoader
import it.unibo.tuprolog.core.Scope
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.operators.OperatorSet
import it.unibo.tuprolog.dsl.logicProgramming
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.library.Library
import it.unibo.tuprolog.solve.rule.RuleWrapper
import it.unibo.tuprolog.theory.Theory

object PassThroughStatementLabeller : ArgLibrary, Loadable {

    override val alias = "prolog.argumentation.graph.labelling.statement.pt"

    override val baseContent: Library
        get() = Library.of(
            alias = this.alias,
            clauses = Theory.of(Labelling.implementation)
        )
    override val baseFlags: Iterable<ArgsFlag<*, *>>
        get() = emptyList()

    override fun identifier(): String = "statement_pass_through"

    override val theoryOperators = DynamicLoader.operators()
        .plus(OperatorSet.DEFAULT)
}

private object Labelling : RuleWrapper<ExecutionContext>("statementLabelling", 0) {
    override val Scope.body: Term
        get() = logicProgramming {
            tupleOf(
                "findall"(
                    `_`,
                    tupleOf(
                        "context_check"("in"(listOf(`_`, `_`, X, `_`, `_`,))),
                        "context_assert"("statIn"(X))
                    ),
                    `_`
                ),
                "findall"(
                    `_`,
                    tupleOf(
                        "context_check"("out"(listOf(`_`, `_`, X, `_`, `_`,))),
                        "context_assert"("statOut"(X))
                    ),
                    `_`
                ),
                "findall"(
                    `_`,
                    tupleOf(
                        "context_check"("und"(listOf(`_`, `_`, X, `_`, `_`,))),
                        "context_assert"("statUnd"(X))
                    ),
                    `_`
                )
            )
        }
}
