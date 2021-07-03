package it.unibo.tuprolog.argumentation.core.mining

import it.unibo.tuprolog.core.Cons
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.dsl.prolog
import it.unibo.tuprolog.solve.Solver
import kotlin.js.JsName

class Attack(val attacker: String, val attacked: String) {
    companion object {
        private fun identifier(argument: Term?, arguments: List<Argument>): String =
            (argument as Cons).toList().let {
                val rules = (it[0] as Cons).toList().map { x -> x.toString() }
                val conclusion = it[2].toString()
                arguments.first { x ->
                    x.rules.containsAll(rules) &&
                        rules.containsAll(x.rules) &&
                        x.conclusion == conclusion
                }.identifier
            }

        @JsName("mineAttacks")
        fun mineAttacks(engine: Solver, arguments: List<Argument>): Sequence<Attack> {
            return prolog {
                engine.solve("attack"(`_`, X, Y))
                    .filter { it.isYes }
                    .map { solution ->
                        Attack(
                            identifier(solution.substitution[X], arguments),
                            identifier(solution.substitution[Y], arguments)
                        )
                    }
            }
        }
    }
}
