package it.unibo.tuprolog.argumentation.core.model.mining

import it.unibo.tuprolog.argumentation.core.dsl.arg2pScope
import it.unibo.tuprolog.argumentation.core.model.Argument
import it.unibo.tuprolog.argumentation.core.model.Attack
import it.unibo.tuprolog.argumentation.core.model.Graph
import it.unibo.tuprolog.argumentation.core.model.LabelledArgument
import it.unibo.tuprolog.argumentation.core.model.Support
import it.unibo.tuprolog.argumentation.core.model.terms.ArgumentTerm
import it.unibo.tuprolog.argumentation.core.model.terms.LabelTerm
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.solve.Solver

// The graph of a context branch, read through graphStore: its arguments, their labels, attacks and supports

/** The graph of the active context branch */
fun Solver.graph(): Graph = graph(activeBranch())

/** The graph of the context branch [context]: its arguments, labelled, their attacks and supports */
fun Solver.graph(context: Int): Graph =
    argumentsAndSupports(context).let { (arguments, supports) ->
        Graph.of(
            labels(context, arguments),
            attacks(context, arguments),
            supports.map { (supporter, supported) ->
                Support(supporter, supported)
            },
        )
    }

/**
 * The arguments of the context branch [context], with the arguments supporting them ([Argument.supports]), identified
 * as A0, A1, ... in order of their number of supporters, rules, conclusion and top rule
 */
fun Solver.arguments(context: Int): List<Argument> = argumentsAndSupports(context).first

// The arguments of the context branch [context], as [arguments], and the supports between them, read once
private fun Solver.argumentsAndSupports(context: Int): Pair<List<Argument>, List<Pair<Argument, Argument>>> {
    val arguments =
        arg2pScope {
            this@argumentsAndSupports
                .solve("graphStore" call "argumentIn"(context, X))
                .filter { it.isYes }
                .map { Argument.of(it.substitution[X]!!) }
                .toList()
        }
    val supports = supportsBetween(context, arguments)
    supports.forEach { (supporter, supported) -> supported.supports.add(supporter) }
    // ordered by the conclusion as tuProlog writes it ('-'(a)), as arguments always were
    val claims = arguments.associateWith { ArgumentTerm.parts(it.termRepresentation())!!.claim.toString() }
    val sorted =
        arguments
            .sortedWith(compareBy({ it.supports.size }, { it.rules.size }, { claims.getValue(it) }, { it.topRule }))
            .onEachIndexed { index, argument -> argument.identifier = "A$index" }
    return sorted to supports
}

/** The attacks between [arguments] in the context branch [context] */
fun Solver.attacks(
    context: Int,
    arguments: List<Argument>,
): List<Attack> {
    if (arguments.isEmpty()) return emptyList()
    val lookup = arguments.lookup()
    return arg2pScope {
        this@attacks
            .solve("graphStore" call "attackIn"(context, T, X, Y, Z))
            .filter { it.isYes }
            .map {
                Attack(
                    lookup.find(it.substitution[X]!!),
                    lookup.find(it.substitution[Y]!!),
                    it.substitution[T]!!.toString(),
                    // none for an attack on no sub-argument in particular
                    lookup.firstOrNull(it.substitution[Z]!!),
                )
            }.toList()
    }
}

/** The supports between [arguments] in the context branch [context] */
fun Solver.supports(
    context: Int,
    arguments: List<Argument>,
): List<Support> = supportsBetween(context, arguments).map { (supporter, supported) -> Support(supporter, supported) }

/** [arguments] with their labels in the context branch [context] (na, when an argument has none) */
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

/** The number of the active context branch: the one of the last stage of the last resolution */
fun Solver.activeBranch(): Int =
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
