package it.unibo.tuprolog.argumentation.causality.libs

import it.unibo.tuprolog.argumentation.core.libs.ArgLibrary
import it.unibo.tuprolog.argumentation.core.libs.ArgsFlag
import it.unibo.tuprolog.argumentation.core.libs.Loadable
import it.unibo.tuprolog.argumentation.core.libs.PrimitiveWithSignature
import it.unibo.tuprolog.argumentation.core.libs.language.RuleParserBase
import it.unibo.tuprolog.core.operators.OperatorSet
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.library.Library
import it.unibo.tuprolog.solve.primitive.Solve

class CausalitySolver : ArgLibrary, Loadable {
    inner class CausalitySolve : PrimitiveWithSignature {
        override val signature = Signature("solve", 1)

        override fun solve(request: Solve.Request<ExecutionContext>): Sequence<Solve.Response> {
            // val goal: Term = request.arguments[0]

            // do something

            return sequenceOf(request.replyWith(true))
        }
    }

    override val alias = "prolog.argumentation.causality.solver"

    override val baseContent: Library
        get() =
            listOf(CausalitySolve()).let { primitives ->
                Library.of(
                    alias = this.alias,
                    primitives = primitives.associateBy { it.signature },
                    operators = RuleParserBase.operators(),
                )
            }

    override val baseFlags: Iterable<ArgsFlag<*, *>>
        get() = emptyList()

    override fun identifier(): String = "causality"

    override var theoryOperators =
        RuleParserBase.operators()
            .plus(OperatorSet.DEFAULT)
}
