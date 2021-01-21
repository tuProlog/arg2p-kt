package it.unibo.tuprolog.argumentation.ui.gui.graph

import it.unibo.tuprolog.core.Cons
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.Var
import it.unibo.tuprolog.dsl.prolog
import it.unibo.tuprolog.solve.Solver

class Attack(val attacker: String, val attacked: String) {
    companion object {
        private fun identifier(argument: Term?, arguments: List<Argument>): String {
            val rules = ((argument as Cons).head as Cons).toList().map { x -> x.toString() }
            return arguments.first { x -> rules.all { r -> x.rules.contains(r) } }.identifier
        }

        fun mineAttacks(engine: Solver, arguments: List<Argument>): Sequence<Attack> {
            return prolog {
                engine.solve("attack"(Var.ANONYMOUS_VAR_NAME, X, Y))
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
