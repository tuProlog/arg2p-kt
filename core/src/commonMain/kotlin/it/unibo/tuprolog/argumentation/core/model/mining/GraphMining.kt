package it.unibo.tuprolog.argumentation.core.model.mining

import it.unibo.tuprolog.argumentation.core.dsl.arg2pScope
import it.unibo.tuprolog.argumentation.core.model.Argument
import it.unibo.tuprolog.argumentation.core.model.Attack
import it.unibo.tuprolog.argumentation.core.model.Graph
import it.unibo.tuprolog.argumentation.core.model.LabelledArgument
import it.unibo.tuprolog.argumentation.core.model.Support
import it.unibo.tuprolog.argumentation.core.model.terms.LabelTerm
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.solve.Solver
import kotlin.js.JsName

// The graph of a context branch, read through graphStore: its arguments, their labels, attacks and supports

/** The graph of the active context branch */
@JsName("mineActiveGraph")
fun Solver.graph(): Graph = graph(activeBranch())

/** The graph of the context branch [context]: its arguments, labelled, their attacks and supports */
@JsName("mineGraph")
fun Solver.graph(context: Int): Graph =
    arguments(context).let {
        Graph.of(labels(context, it), attacks(context, it), supports(context, it))
    }

/**
 * The arguments of the context branch [context], with the arguments supporting them ([Argument.supports]), identified
 * as A0, A1, ... in order of their number of supporters, rules, conclusion and top rule
 */
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
    supportsBetween(context, arguments).forEach { (supporter, supported) -> supported.supports.add(supporter) }
    return arguments
        .sortedWith(compareBy({ it.supports.size }, { it.rules.size }, { it.conclusion }, { it.topRule }))
        .onEachIndexed { index, argument -> argument.identifier = "A$index" }
}

/** The attacks between [arguments] in the context branch [context] */
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
            .map { Attack(lookup.find(it.substitution[X]!!), lookup.find(it.substitution[Y]!!)) }
            .toList()
    }
}

/** The supports between [arguments] in the context branch [context] */
@JsName("mineSupports")
fun Solver.supports(
    context: Int,
    arguments: List<Argument>,
): List<Support> = supportsBetween(context, arguments).map { (supporter, supported) -> Support(supporter, supported) }

/** [arguments] with their labels in the context branch [context] (na, when an argument has none) */
@JsName("mineLabels")
fun Solver.labels(
    context: Int,
    arguments: List<Argument>,
): List<LabelledArgument> {
    val labelled =
        LabelTerm.labels.flatMap { label ->
            arg2pScope {
                this@labels
                    .solve("graphStore" call "argumentLabelIn"(context, label, X))
                    .filter { it.isYes }
                    .map { Pair(it.substitution[X]!!, label) }
                    .toList()
            }
        }
    val labels = TermLookup(labelled) { it.first }
    return arguments.map { LabelledArgument(it, labels.firstOrNull(it.termRepresentation())?.second ?: "na") }
}

// The number of the active context branch
private fun Solver.activeBranch(): Int =
    arg2pScope {
        this@activeBranch
            .solve("context_active"(X))
            .filter { it.isYes }
            .map { it.substitution[X]!!.toString().toInt() }
            .firstOrNull()
    } ?: error("couldn't find a graph")

// The pairs of supporter and supported argument, among [arguments], in the context branch [context]
private fun Solver.supportsBetween(
    context: Int,
    arguments: List<Argument>,
): List<Pair<Argument, Argument>> {
    val lookup = arguments.lookup()
    return arg2pScope {
        this@supportsBetween
            .solve("graphStore" call "supportIn"(context, X, Y))
            .filter { it.isYes }
            .map { Pair(lookup.find(it.substitution[X]!!), lookup.find(it.substitution[Y]!!)) }
            .toList()
    }
}

private fun List<Argument>.lookup() = TermLookup(this) { it.termRepresentation() }

private fun TermLookup<Argument>.find(term: Term) = firstOrNull(term) ?: throw NoSuchElementException("No argument matches $term")
