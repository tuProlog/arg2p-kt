package it.unibo.tuprolog.argumentation.core.mining

import it.unibo.tuprolog.core.Cons
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.Var
import it.unibo.tuprolog.dsl.prolog
import it.unibo.tuprolog.solve.Solver
import it.unibo.tuprolog.unify.Unificator
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
            return prolog{
                engine.solve("cache_check"("graph"(listOf(Var.anonymous(), X, Var.anonymous()))))
                    .map { (it.substitution[X] as Cons).toSequence() }
                    .first()
                    .map {
                        Unificator.default.mgu(it, tupleOf(`_`, Z, Y, `_`))
                    }.filter { it.isSuccess }.map { solution ->
                        Attack(
                            identifier(solution[Z], arguments),
                            identifier(solution[Y], arguments)
                        )
                    }
            }
        }
    }
}
