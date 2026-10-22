package it.unibo.tuprolog.argumentation.core.mining

import it.unibo.tuprolog.argumentation.core.dsl.arg2pScope
import it.unibo.tuprolog.argumentation.core.libs.data.LabelTerm
import it.unibo.tuprolog.argumentation.core.model.Argument
import it.unibo.tuprolog.argumentation.core.model.Attack
import it.unibo.tuprolog.argumentation.core.model.Graph
import it.unibo.tuprolog.argumentation.core.model.LabelledArgument
import it.unibo.tuprolog.argumentation.core.model.Support
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.solve.Solver
import it.unibo.tuprolog.unify.Unificator
import kotlin.js.JsName

@JsName("mineActiveGraph")
fun Solver.graph() =
    arg2pScope {
        this@graph
            .solve("context_active"(X))
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

// The arguments, identified as A0, A1, ... in order of their number of sub-arguments (supports), rules, conclusion and
// top rule
@JsName("mineArguments")
fun Solver.arguments(context: Int): List<Argument> {
    val arguments =
        arg2pScope {
            this@arguments
                .solve("graphStore" call "argumentIn"(context, X))
                .filter { it.isYes }
                .map { Argument.of(it.substitution[X]!!) }
                .toList()
        }
    val supports = supportCounts(context, arguments)
    return arguments
        .sortedWith(compareBy({ supports[it] ?: 0 }, { it.rules.size }, { it.conclusion }, { it.topRule }))
        .onEachIndexed { index, argument -> argument.identifier = "A$index" }
}

// The number of arguments supporting each argument
private fun Solver.supportCounts(
    context: Int,
    arguments: List<Argument>,
): Map<Argument, Int> {
    val lookup = arguments.lookup()
    return arg2pScope {
        this@supportCounts
            .solve("graphStore" call "supportIn"(context, `_`, Y))
            .filter { it.isYes }
            .mapNotNull { lookup.firstOrNull(it.substitution[Y]!!) }
            .groupingBy { it }
            .eachCount()
    }
}

@JsName("mineAttacks")
fun Solver.attacks(
    context: Int,
    arguments: List<Argument>,
): List<Attack> {
    if (arguments.isEmpty()) return emptyList()
    val lookup = arguments.lookup()
    return arg2pScope {
        this@attacks
            .solve("graphStore" call "attackIn"(context, `_`, X, Y, `_`))
            .filter { it.isYes }
            .map { Pair(it.substitution[X]!!, it.substitution[Y]!!) }
            .map { solution ->
                Attack(
                    lookup.find(solution.first),
                    lookup.find(solution.second),
                )
            }
    }.toList()
}

@JsName("mineSupports")
fun Solver.supports(
    context: Int,
    arguments: List<Argument>,
): List<Support> {
    val lookup = arguments.lookup()
    return arg2pScope {
        this@supports
            .solve("graphStore" call "supportIn"(context, X, Y))
            .filter { it.isYes }
            .map { Pair(it.substitution[X]!!, it.substitution[Y]!!) }
            .map { solution ->
                lookup.find(solution.second).let { argument ->
                    Support(
                        lookup.find(solution.first),
                        argument,
                    ).also {
                        argument.supports.add(it.supporter)
                    }
                }
            }.toList()
    }
}

@JsName("mineLabels")
fun Solver.labels(
    context: Int,
    arguments: List<Argument>,
): List<LabelledArgument> {
    fun labelled(label: String) =
        arg2pScope {
            this@labels
                .solve("graphStore" call "argumentLabelIn"(context, label, X))
                .filter { it.isYes }
                .map { Pair(it.substitution[X]!!, label) }
                .toList()
        }

    val labels = TermLookup(LabelTerm.labels.flatMap { labelled(it) }) { it.first }
    return arguments.map { res ->
        LabelledArgument(
            res,
            labels.firstOrNull(res.termRepresentation())?.second ?: "na",
        )
    }
}

// Mined terms are matched through unification, which amounts to structural equality when both sides are ground:
// in that case a hash lookup replaces the scan of all the candidates for every attack, support and label
private class TermLookup<T>(
    items: List<T>,
    key: (T) -> Term,
) {
    private val entries = items.map { key(it) to it }

    private val index: Map<Term, T>? =
        if (entries.all { it.first.isGround }) {
            mutableMapOf<Term, T>().also { map -> entries.forEach { (term, item) -> map.getOrPut(term) { item } } }
        } else {
            null
        }

    fun firstOrNull(term: Term): T? =
        if (index != null && term.isGround) {
            index[term]
        } else {
            entries.firstOrNull { Unificator.default.match(it.first, term) }?.second
        }
}

private fun List<Argument>.lookup() = TermLookup(this) { it.termRepresentation() }

private fun TermLookup<Argument>.find(term: Term) = firstOrNull(term) ?: throw NoSuchElementException("No argument matches $term")
