package it.unibo.tuprolog.argumentation.causality.runtime

import it.unibo.tuprolog.argumentation.core.Arg2pSolver
import it.unibo.tuprolog.argumentation.core.Arg2pSolverFactory
import it.unibo.tuprolog.argumentation.core.dsl.arg2pScope
import it.unibo.tuprolog.argumentation.core.model.Argument
import it.unibo.tuprolog.argumentation.core.model.Graph
import it.unibo.tuprolog.argumentation.core.model.LabelledArgument
import it.unibo.tuprolog.argumentation.core.model.mining.graph
import it.unibo.tuprolog.argumentation.core.runtime.settings.FlagsBuilder
import it.unibo.tuprolog.core.Atom
import it.unibo.tuprolog.core.Clause
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.primitive.Solve
import it.unibo.tuprolog.theory.Theory
import it.unibo.tuprolog.theory.parsing.parse
import it.unibo.tuprolog.unify.Unificator
import it.unibo.tuprolog.core.List as PlList

private const val GRAPH_CACHE_SIZE = 256

/**
 * The causal analysis of a theory, for the primitives of the causality module: the graphs of the theory and of its
 * interventions, labelled with grounded semantics and no graph extensions by a solver of its own, and the NESS and
 * but-for checks on them. The theory is the static knowledge base of the solver asking ([Solve.Request]).
 */
internal class CausalityAnalysis {
    private val solver =
        Arg2pSolverFactory.default(
            settings = FlagsBuilder(graphExtensions = emptyList(), argumentLabellingMode = "grounded").create(),
        )

    private val operators = Arg2pSolver.default().operators()

    // The same theory is solved over and over (the base theory, or the same intervention in different checks): the
    // graphs are only read afterwards, hence they are shared
    private val graphCache = mutableMapOf<Pair<String, String>, Graph>()

    /** The graph of [theory] for [query], labelled */
    private fun graphOf(
        theory: Theory,
        query: Term,
    ): Graph {
        val text = theory.toString(asPrologText = true)
        val key = text to query.toString()
        graphCache[key]?.let { return it }
        if (graphCache.size >= GRAPH_CACHE_SIZE) graphCache.clear()
        solver.resetStaticKb()
        solver.loadStaticKb(Theory.parse(text, operators))
        return (
            solver
                .solve(Struct.of("answerQuery", query))
                .map { solver.graph() }
                .firstOrNull() ?: Graph(emptyList(), emptyList(), emptyList())
        ).also { graphCache[key] = it }
    }

    private fun concludes(
        argument: Argument,
        statement: Term,
    ) = Unificator.default.match(Term.parse(argument.conclusion, operators), statement)

    private fun attackers(
        argument: Argument,
        graph: Graph,
    ): List<LabelledArgument> =
        graph.attacks
            .filter { it.target.identifier == argument.identifier }
            .map { attack -> graph.labellings.first { attack.attacker.identifier == it.argument.identifier } }

    // The arguments IN defending argument along the attacks: a grounded chain explaining its label
    private fun supporters(
        argument: Argument,
        graph: Graph,
        chain: List<String>,
    ): List<LabelledArgument> =
        if (argument.identifier in chain) {
            emptyList()
        } else {
            graph.labellings.filter { argument.identifier == it.argument.identifier && it.label == "in" } +
                attackers(argument, graph).flatMap { attacker ->
                    attackers(attacker.argument, graph)
                        .filter { it.argument.identifier != argument.identifier }
                        .flatMap { supporters(it.argument, graph, chain + argument.identifier) }
                }
        }

    // The support set of effect: the union of all its explanations
    private fun supports(
        graph: Graph,
        effect: Term,
    ): Sequence<Argument> =
        graph.labellings
            .asSequence()
            .filter { concludes(it.argument, effect) }
            .flatMap { supporters(it.argument, graph, emptyList()) }
            .map { it.argument }

    private fun theory(request: Solve.Request<ExecutionContext>) = request.context.staticKb

    // The theory with the statements of intervention as axioms, each named after its statement
    private fun intervened(
        request: Solve.Request<ExecutionContext>,
        intervention: PlList,
    ): Theory =
        theory(request).plus(
            Theory.of(
                intervention.toList().map { statement ->
                    Clause.of(Struct.of(":->", Atom.of("fk${statement.hashCode() and Int.MAX_VALUE}"), statement))
                },
            ),
        )

    /** Some argument for [statement] is IN in the grounded labelling of the theory */
    fun isAccepted(
        request: Solve.Request<ExecutionContext>,
        statement: Term,
    ): Boolean = graphOf(theory(request), statement).labellings.any { it.label == "in" && concludes(it.argument, statement) }

    private fun baseSupports(
        request: Solve.Request<ExecutionContext>,
        effect: Term,
    ): List<Term> = supports(graphOf(theory(request), effect), effect).map { it.termRepresentation() }.toList()

    // The IN attackers, after intervention, of the supports of effect it makes OUT or UND
    private fun interventionAttackers(
        request: Solve.Request<ExecutionContext>,
        intervention: PlList,
        supports: List<Term>,
        effect: Term,
    ): List<Argument> =
        graphOf(intervened(request, intervention), effect).let { graph ->
            graph.labellings
                .filter { it.label == "out" || it.label == "und" }
                .filter { a -> supports.any { b -> Unificator.default.match(a.argument.termRepresentation(), b) } }
                .flatMap { a -> attackers(a.argument, graph).filter { it.label == "in" } }
                .map { it.argument }
                .distinct()
        }

    private fun properSubsets(
        request: Solve.Request<ExecutionContext>,
        intervention: PlList,
    ): List<PlList> =
        arg2pScope {
            request
                .solve("properSubsets"(intervention, X))
                .filter { it.isYes }
                .map { it.substitution[X]!!.castToList() }
                .toList()
        }

    /** The interventions removing [cause], on the rules of the graph of [effect] */
    fun interventions(
        request: Solve.Request<ExecutionContext>,
        cause: Term,
        effect: Term,
    ): Sequence<PlList> {
        val rules =
            PlList.of(
                graphOf(theory(request), effect)
                    .arguments
                    .map { it.topRule }
                    .filter { it != "none" }
                    .map { Term.parse(it, operators) },
            )
        return arg2pScope {
            request
                .solve("generateInterventions"(rules, cause, X))
                .filter { it.isYes }
                .map { it.substitution[X]!!.castToList() }
        }
    }

    /**
     * The NESS condition of ICAIL 2025: an explanation of [effect] (a grounded chain) is refuted by [intervention],
     * and by none of its proper subsets
     */
    fun nessOriginal(
        request: Solve.Request<ExecutionContext>,
        intervention: PlList,
        effect: Term,
    ): Boolean {
        val supports = baseSupports(request, effect)
        if (supports.isEmpty()) return false
        val attackers = interventionAttackers(request, intervention, supports, effect)
        return attackers.isNotEmpty() &&
            (
                intervention.estimatedLength <= 1 ||
                    properSubsets(request, intervention).none { interventionAttackers(request, it, supports, effect).isNotEmpty() }
            )
    }

    /**
     * The revised NESS condition: an explanation of [effect] is refuted by [intervention] (an IN attacker), and no
     * proper subset of it refutes it with the same attackers
     */
    fun ness(
        request: Solve.Request<ExecutionContext>,
        intervention: PlList,
        effect: Term,
    ): Boolean {
        val supports = baseSupports(request, effect)
        if (supports.isEmpty()) return false
        val attackers = interventionAttackers(request, intervention, supports, effect).map { it.termRepresentation() }
        return attackers.isNotEmpty() &&
            (
                intervention.estimatedLength <= 1 ||
                    properSubsets(request, intervention).none { subset ->
                        val others = interventionAttackers(request, subset, supports, effect).map { it.termRepresentation() }
                        attackers.all { a -> others.any { b -> Unificator.default.match(a, b) } }
                    }
            )
    }

    /** The but-for condition: an argument for [effect] is IN, and none is after [intervention] */
    fun butFor(
        request: Solve.Request<ExecutionContext>,
        intervention: PlList,
        effect: Term,
    ): Boolean =
        isAccepted(request, effect) &&
            graphOf(intervened(request, intervention), effect).labellings.none {
                it.label == "in" && concludes(it.argument, effect)
            }
}
