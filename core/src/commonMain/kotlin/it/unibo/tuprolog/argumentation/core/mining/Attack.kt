package it.unibo.tuprolog.argumentation.core.mining

import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.dsl.prolog
import it.unibo.tuprolog.solve.Solver
import kotlin.js.JsName

class Attack(val attacker: String, val attacked: String) {
    companion object {
        private fun identifier(argument: Term, arguments: List<Argument>): String =
            arguments.first { it.term == argument }.identifier

        @JsName("mineAttacks")
        fun mineAttacks(context: Int, engine: Solver, arguments: List<Argument>): List<Attack> {
            if (arguments.isEmpty()) return emptyList()
            return prolog {
                engine.solve("context_check"(context, "attack"(`_`, X, Y, `_`)))
                    .filter { it.isYes }
                    .map { solution ->
                        Attack(
                            identifier(solution.substitution[X]!!, arguments),
                            identifier(solution.substitution[Y]!!, arguments)
                        )
                    }
            }.toList()
        }
    }
}
