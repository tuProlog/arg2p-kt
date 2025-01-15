package it.unibo.tuprolog.argumentation.core.mining

import it.unibo.tuprolog.argumentation.core.model.Argument
import it.unibo.tuprolog.argumentation.core.model.Attack
import it.unibo.tuprolog.argumentation.core.model.Graph
import it.unibo.tuprolog.argumentation.core.model.LabelledArgument
import it.unibo.tuprolog.argumentation.core.model.Support
import it.unibo.tuprolog.dsl.logicProgramming
import it.unibo.tuprolog.solve.Solver
import it.unibo.tuprolog.unify.Unificator
import kotlin.js.JsName

@JsName("mineActiveGraph")
fun Solver.graph() =
    logicProgramming {
        this@graph.solve("context_active"(X))
            .filter { it.isYes }
            .map { it.substitution[X]!!.toString().toInt() }
            .map { context -> this@graph.graph(context) }
            .firstOrNull() ?: error("couldn't find a graph")
    }

@JsName("mineGraph")
fun Solver.graph(context: Int) =
    this.arguments(context).let {
        Graph.of(
            this.labels(context, it),
            this.attacks(context, it),
            this.supports(context, it),
        )
    }

@JsName("mineArguments")
fun Solver.arguments(context: Int): List<Argument> =
    logicProgramming {
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
fun Solver.attacks(
    context: Int,
    arguments: List<Argument>,
): List<Attack> {
    if (arguments.isEmpty()) return emptyList()
    return logicProgramming {
        this@attacks.solve("context_check"(context, "attack"(`_`, X, Y, `_`)))
            .filter { it.isYes }
            .map { Pair(it.substitution[X]!!, it.substitution[Y]!!) }
            .map { solution ->
                Attack(
                    arguments.first { Unificator.default.match(it.termRepresentation(), solution.first) },
                    arguments.first { Unificator.default.match(it.termRepresentation(), solution.second) },
                )
            }
    }.toList()
}

@JsName("mineSupports")
fun Solver.supports(
    context: Int,
    arguments: List<Argument>,
): List<Support> =
    logicProgramming {
        this@supports.solve("context_check"(context, "support"(X, Y)))
            .filter { it.isYes }
            .map { Pair(it.substitution[X]!!, it.substitution[Y]!!) }
            .map { solution ->
                arguments.first { Unificator.default.match(it.termRepresentation(), solution.second) }.let { argument ->
                    Support(
                        arguments.first { Unificator.default.match(it.termRepresentation(), solution.first) },
                        argument,
                    ).also {
                        argument.supports.add(it.supporter)
                    }
                }
            }.toList()
    }

@JsName("mineLabels")
fun Solver.labels(
    context: Int,
    arguments: List<Argument>,
): List<LabelledArgument> {
    fun checkFunctor(functor: String) =
        logicProgramming {
            this@labels.solve("context_check"(context, functor(X)))
                .filter { it.isYes }
                .map {
                        res ->
                    LabelledArgument(arguments.first { Unificator.default.match(it.termRepresentation(), res.substitution[X]!!) }, functor)
                }
                .toList()
        }

    return checkFunctor("in") + checkFunctor("out") + checkFunctor("und")
}
