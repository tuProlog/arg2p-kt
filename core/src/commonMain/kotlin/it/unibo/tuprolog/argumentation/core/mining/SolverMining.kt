package it.unibo.tuprolog.argumentation.core.mining

import it.unibo.tuprolog.argumentation.core.model.Argument
import it.unibo.tuprolog.argumentation.core.model.Attack
import it.unibo.tuprolog.argumentation.core.model.Graph
import it.unibo.tuprolog.argumentation.core.model.LabelledArgument
import it.unibo.tuprolog.argumentation.core.model.Support
import it.unibo.tuprolog.dsl.prolog
import it.unibo.tuprolog.solve.Solver
import it.unibo.tuprolog.unify.Unificator
import kotlin.js.JsName

@JsName("mineGraph")
fun Solver.graph(context: Int) =
    this.arguments(context).let {
        Graph.of(
            this.labels(context, it),
            this.attacks(context, it),
            this.supports(context, it)
        )
    }

@JsName("mineArguments")
fun Solver.arguments(context: Int): List<Argument> =
    prolog {
        this@arguments.solve("context_check"(context, "argument"(X)))
            .filter { it.isYes }
            .map { it.substitution[X]!! }
            .map { solution ->
                Argument.of(solution)
            }
            .sortedWith(compareBy({ it.supports.size }, { it.rules.size }, { it.conclusion }, { it.topRule }))
            .mapIndexed { index, arg ->
                arg.identifier = "A$index"
                arg
            }
    }.toList()

@JsName("mineAttacks")
fun Solver.attacks(context: Int, arguments: List<Argument>): List<Attack> {
    if (arguments.isEmpty()) return emptyList()
    return prolog {
        this@attacks.solve("context_check"(context, "attack"(`_`, X, Y, `_`)))
            .filter { it.isYes }
            .map { solution ->
                Attack(
                    arguments.first { Unificator.default.match(it.termRepresentation(), solution.substitution[X]!!) },
                    arguments.first { Unificator.default.match(it.termRepresentation(), solution.substitution[Y]!!) }
                )
            }
    }.toList()
}

@JsName("mineSupports")
fun Solver.supports(context: Int, arguments: List<Argument>): List<Support> =
    arguments.flatMap { argument ->
        prolog {
            this@supports.solve("context_check"(context, "support"(X, argument.termRepresentation())))
                .filter { it.isYes }
                .map { it.substitution[X]!! }
                .map { solution ->
                    Support(
                        arguments.first { Unificator.default.match(it.termRepresentation(), solution) },
                        argument
                    ).also {
                        argument.supports.add(it.supporter)
                    }
                }.toList()
        }
    }

@JsName("mineLabels")
fun Solver.labels(context: Int, arguments: List<Argument>): List<LabelledArgument> {
    fun checkFunctor(functor: String, argument: Argument) =
        prolog {
            this@labels.solve("context_check"(context, functor(argument.termRepresentation())))
                .filter { it.isYes }
                .map { functor }
                .firstOrNull()
        }

    return arguments.map {
        LabelledArgument(
            it,
            checkFunctor("in", it)
                ?: checkFunctor("out", it) ?: "und"
        )
    }
}
