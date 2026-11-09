package it.unibo.tuprolog.argumentation.causality.helpers

import it.unibo.tuprolog.argumentation.Flags
import it.unibo.tuprolog.argumentation.Semantics
import it.unibo.tuprolog.argumentation.core.Arg2pSolver
import it.unibo.tuprolog.argumentation.core.Arg2pSolverFactory
import it.unibo.tuprolog.argumentation.core.dsl.arg2pScope
import it.unibo.tuprolog.argumentation.core.libraries.evaluation.phases.flags.ArgumentLabellingMode
import it.unibo.tuprolog.argumentation.core.libraries.framework.settings.Settings
import it.unibo.tuprolog.argumentation.core.libraries.language.parser.helpers.RuleAnnotations
import it.unibo.tuprolog.argumentation.core.libraries.language.parser.helpers.TermValues
import it.unibo.tuprolog.argumentation.core.libraries.store.helpers.mining.graph
import it.unibo.tuprolog.argumentation.core.model.Argument
import it.unibo.tuprolog.argumentation.core.model.Graph
import it.unibo.tuprolog.argumentation.core.model.LabelledArgument
import it.unibo.tuprolog.argumentation.engine.term.Arg2pUnificator
import it.unibo.tuprolog.core.Atom
import it.unibo.tuprolog.core.Clause
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.MutableSolver
import it.unibo.tuprolog.solve.Solution
import it.unibo.tuprolog.solve.primitive.Solve
import it.unibo.tuprolog.theory.Theory
import it.unibo.tuprolog.theory.parsing.parse
import it.unibo.tuprolog.core.List as PlList

private const val GRAPH_CACHE_SIZE = 256

/**
 * The causal analysis of a theory, for the primitives of the causality module: the graphs of the theory and of its
 * interventions, labelled by a solver of its own with grounded semantics (the other flags those of the solver asking),
 * and the NESS and but-for checks on them. The theory is the static knowledge base of the solver asking
 * ([Solve.Request]); what it prints goes to the standard output of that solver.
 */
internal class CausalityAnalysis {
    // The solver labelling the graphs, built at the first of them: a solver of arg2p not testing causality pays nothing
    private var labelling: MutableSolver? = null

    // The solver labelling the graphs, printing where the solver asking does
    private fun solverFor(request: Solve.Request<ExecutionContext>): MutableSolver {
        val output = request.context.standardOutput
        val solver =
            labelling ?: Arg2pSolverFactory
                .solverOf(
                    Arg2pSolver.default(listOf(Flags.defaults().argumentLabellingMode(Semantics.GROUNDED).library())),
                    theory = "",
                    output = output,
                ).also { labelling = it }
        if (solver.standardOutput !== output) solver.setStandardOutput(output)
        return solver
    }

    // The same theory is solved over and over (the base theory, or the same intervention in different checks): the
    // graphs are only read afterwards, hence they are shared
    private val graphCache = mutableMapOf<Pair<String, String>, Reading>()

    // What the checks ask of a graph, over and over
    private class Reading(
        val graph: Graph,
    ) {
        private val labelled = graph.labellings.associateBy { it.argument.identifier }

        private val attackersOf: Map<String, List<LabelledArgument>> =
            graph.attacks
                .groupBy { it.target.identifier }
                .mapValues { (_, attacks) -> attacks.mapNotNull { labelled[it.attacker.identifier] } }

        fun attackers(argument: Argument): List<LabelledArgument> = attackersOf[argument.identifier].orEmpty()

        fun inLabelled(argument: Argument): LabelledArgument? = labelled[argument.identifier]?.takeIf { it.label == "in" }
    }

    // The conclusions of the arguments as terms, read once: the same few in all the graphs
    private val conclusions = mutableMapOf<String, Term>()

    /** The graph of [theory] for [query], labelled */
    private fun graphOf(
        request: Solve.Request<ExecutionContext>,
        theory: Theory,
        query: Term,
    ): Reading {
        // the identifiers of the clauses too: the same text is a different theory with them (see RuleAnnotations)
        val text = theory.toString(asPrologText = true) + theory.joinToString(",") { RuleAnnotations.of(it).orEmpty() }
        val key = text to query.toString()
        graphCache[key]?.let { return it }
        if (graphCache.size >= GRAPH_CACHE_SIZE) {
            graphCache.clear()
            conclusions.clear()
        }
        val solver = solverFor(request)
        solver.resetStaticKb()
        // the theory as it is, not parsed again from its text: its clauses keep their tags
        solver.loadStaticKb(theory)
        val solution = solver.solve(Struct.of("solve", query)).first()
        // an error is not a graph: raised again, and not kept
        if (solution is Solution.Halt) throw solution.exception
        return Reading(solver.graph()).also { graphCache[key] = it }
    }

    private fun concludes(
        argument: Argument,
        statement: Term,
    ) = Arg2pUnificator.match(
        conclusions.getOrPut(argument.conclusion) { Term.parse(argument.conclusion, TermValues.operators) },
        statement,
    )

    // The arguments IN defending argument along the attacks: a grounded chain explaining its label
    private fun supporters(
        argument: Argument,
        reading: Reading,
        chain: List<String>,
    ): List<LabelledArgument> =
        if (argument.identifier in chain) {
            emptyList()
        } else {
            listOfNotNull(reading.inLabelled(argument)) +
                reading.attackers(argument).flatMap { attacker ->
                    reading
                        .attackers(attacker.argument)
                        .filter { it.argument.identifier != argument.identifier }
                        .flatMap { supporters(it.argument, reading, chain + argument.identifier) }
                }
        }

    // The support set of effect: the union of all its explanations
    private fun supports(
        reading: Reading,
        effect: Term,
    ): Sequence<Argument> =
        reading.graph.labellings
            .asSequence()
            .filter { concludes(it.argument, effect) }
            .flatMap { supporters(it.argument, reading, emptyList()) }
            .map { it.argument }

    // The theory of the solver asking, with its flags (those of its libraries too) as facts, but the semantics, which
    // is that of the analysis: its own flag facts give way to them
    private fun theory(request: Solve.Request<ExecutionContext>): Theory {
        val theory = request.context.staticKb
        val settings = Settings.of(request.context) ?: return theory
        val except = setOf(ArgumentLabellingMode.predicate())
        return Theory.of(
            theory.unificator,
            theory.clauses.filterNot { settings.states(it) } + settings.facts(request.context, except),
        )
    }

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
    ): Boolean = graphOf(request, theory(request), statement).graph.labellings.any { it.label == "in" && concludes(it.argument, statement) }

    private fun baseSupports(
        request: Solve.Request<ExecutionContext>,
        effect: Term,
    ): List<Term> = supports(graphOf(request, theory(request), effect), effect).map { it.termRepresentation() }.toList()

    // The IN attackers, after intervention, of the supports of effect it makes OUT or UND
    private fun interventionAttackers(
        request: Solve.Request<ExecutionContext>,
        intervention: PlList,
        supports: List<Term>,
        effect: Term,
    ): List<Argument> =
        graphOf(request, intervened(request, intervention), effect).let { reading ->
            // a ground argument only unifies with itself, or with a support having variables
            val groundSupports = supports.filterTo(HashSet()) { it.isGround }
            val openSupports = supports.filterNot { it.isGround }
            reading.graph.labellings
                .filter { it.label == "out" || it.label == "und" }
                .filter { a ->
                    val term = a.argument.termRepresentation()
                    if (term.isGround) {
                        term in groundSupports || openSupports.any { Arg2pUnificator.match(term, it) }
                    } else {
                        supports.any { Arg2pUnificator.match(term, it) }
                    }
                }.flatMap { a -> reading.attackers(a.argument).filter { it.label == "in" } }
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
                graphOf(request, theory(request), effect)
                    .graph.arguments
                    .map { it.topRule }
                    .filter { it != "none" }
                    .map { Term.parse(it, TermValues.operators) },
            )
        return arg2pScope {
            request
                .solve("generateInterventions"(rules, cause, X))
                .filter { it.isYes }
                .map { it.substitution[X]!!.castToList() }
        }
    }

    /**
     * The NESS condition: an explanation of [effect] (a grounded chain) is refuted by [intervention] (an IN attacker),
     * and by none of its proper subsets: as published (ICAIL 2025), a subset refuting it any way; [revised], a subset
     * refuting it with the same attackers
     */
    fun ness(
        request: Solve.Request<ExecutionContext>,
        intervention: PlList,
        effect: Term,
        revised: Boolean,
    ): Boolean {
        val supports = baseSupports(request, effect)
        if (supports.isEmpty()) return false
        val attackers = interventionAttackers(request, intervention, supports, effect).map { it.termRepresentation() }
        if (attackers.isEmpty()) return false
        if (intervention.estimatedLength <= 1) return true
        return properSubsets(request, intervention).none { subset ->
            val others = interventionAttackers(request, subset, supports, effect).map { it.termRepresentation() }
            if (revised) attackers.all { a -> others.any { b -> Arg2pUnificator.match(a, b) } } else others.isNotEmpty()
        }
    }

    /** The but-for condition: an argument for [effect] is IN, and none is after [intervention] */
    fun butFor(
        request: Solve.Request<ExecutionContext>,
        intervention: PlList,
        effect: Term,
    ): Boolean =
        isAccepted(request, effect) &&
            graphOf(request, intervened(request, intervention), effect).graph.labellings.none {
                it.label == "in" && concludes(it.argument, effect)
            }
}
