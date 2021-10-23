package it.unibo.tuprolog.argumentation.core.libs.graph.labelling

import it.unibo.tuprolog.argumentation.core.libs.ArgsFlag
import it.unibo.tuprolog.argumentation.core.libs.BaseArgLibrary
import it.unibo.tuprolog.argumentation.core.libs.Loadable
import it.unibo.tuprolog.core.Scope
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.dsl.prolog
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.library.AliasedLibrary
import it.unibo.tuprolog.solve.library.Library
import it.unibo.tuprolog.solve.rule.RuleWrapper
import it.unibo.tuprolog.theory.Theory

object BinaryStatementLabeller : BaseArgLibrary(), Loadable {

    override val alias = "prolog.argumentation.graph.labelling.statement.binary"

    override val baseContent: AliasedLibrary
        get() = Library.aliased(
            alias = this.alias,
            theory = Theory.of(BinaryLabelling.implementation)
        )
    override val baseFlags: Iterable<ArgsFlag<*, *>>
        get() = emptyList()

    override fun identifier(): String = "statement_binary"
}

private object BinaryLabelling : RuleWrapper<ExecutionContext>("statementLabelling", 0) {
    override val Scope.body: Term
        get() = prolog {
            tupleOf(
                "findall"(
                    `_`,
                    tupleOf(
                        "context_check"("in"(listOf(`_`, `_`, X, `_`, `_`,))),
                        not("contextCheck"("statIn"(X))),
                        "context_assert"("statIn"(X))
                    ),
                    `_`
                ),
                "findall"(
                    `_`,
                    tupleOf(
                        "context_check"("out"(listOf(`_`, `_`, X, `_`, `_`,))) or "context_check"("und"(listOf(`_`, `_`, X, `_`, `_`,))),
                        not("context_check"("statOut"(X))),
                        "context_assert"("statOut"(X))
                    ),
                    `_`
                )
            )
        }
}
