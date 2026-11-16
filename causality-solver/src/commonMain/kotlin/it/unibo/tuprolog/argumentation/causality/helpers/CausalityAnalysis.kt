package it.unibo.tuprolog.argumentation.causality.helpers

import it.unibo.tuprolog.argumentation.Flags
import it.unibo.tuprolog.argumentation.Semantics
import it.unibo.tuprolog.argumentation.TermValues
import it.unibo.tuprolog.argumentation.core.Arg2pSolver
import it.unibo.tuprolog.argumentation.core.Arg2pSolverFactory
import it.unibo.tuprolog.argumentation.core.dsl.arg2pScope
import it.unibo.tuprolog.argumentation.core.libraries.evaluation.pipeline.flags.ArgumentLabellingMode
import it.unibo.tuprolog.argumentation.core.libraries.framework.parser.helpers.RuleAnnotations
import it.unibo.tuprolog.argumentation.core.libraries.framework.settings.Settings
import it.unibo.tuprolog.argumentation.core.libraries.framework.store.helpers.mining.graph
import it.unibo.tuprolog.argumentation.core.model.Argument
import it.unibo.tuprolog.argumentation.core.model.Graph
import it.unibo.tuprolog.argumentation.core.model.LabelledArgument
import it.unibo.tuprolog.argumentation.engine.term.Arg2pUnificator
import it.unibo.tuprolog.core.Atom
import it.unibo.tuprolog.core.Clause
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Substitution
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
            keys.clear()
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
     * The explanations of [argument], IN in [reading]: its subset-minimal strongly admissible sets within the IN arguments
     * (Def. 3.3 of the ICAIL 2025 paper, for finite frameworks): the argument, and for each of its defeaters one IN
     * defender, defended in turn the same way, never by an argument it defends
     */
    private fun explanations(
        reading: Reading,
        argument: Argument,
    ): List<Set<Argument>> {
        fun build(
            a: Argument,
            path: Set<String>,
        ): List<Set<Argument>> {
            val inside = path + a.identifier
            var sets = listOf(setOf(a))
            for (defeater in reading.attackers(a)) {
                val defenders =
                    reading.attackers(defeater.argument).filter { it.label == "in" && it.argument.identifier !in inside }
                sets =
                    sets.flatMap { set ->
                        if (defenders.any { it.argument in set }) {
                            listOf(set)
                        } else {
                            defenders.flatMap { d -> build(d.argument, inside).map { set + it } }
                        }
                    }
                if (sets.isEmpty()) return emptyList()
            }
            return sets
        }
        val all = build(argument, emptySet()).distinct()
        return all.filter { set -> all.none { other -> other != set && set.containsAll(other) } }
    }

    // An argument as the same one in the graph of another theory: its shape (see shapeOf) and its info (its last
    // defeasible rules, its defeasible rules and premises), as sets, not its term, whose order follows how the graph was
    // built and whose groundings carry the variables of the bodies of its rules, named apart in each graph; each
    // identifier with its variables named in order (a rule used for any value, r(X), has an identifier with a variable)
    private val keys = mutableMapOf<Argument, List<Any>>()

    private fun keyOf(argument: Argument) =
        keys.getOrPut(argument) {
            val constraints = argument.groundings.filter { it.startsWith("{") }
            listOf(
                canonical("k(${argument.conclusion}, [${constraints.joinToString(", ")}])"),
                argument.lastDefeasibleRules.map(::canonical).toSet(),
                argument.defeasibleRules.map(::canonical).toSet(),
                argument.defeasiblePremises.map(::canonical).toSet(),
            )
        }

    // The text of a term with its variables named in the order they are met: the same up to the names of its variables
    // (an open conclusion, holdsAt(f, T), with its constraints, the piece of a period, read together)
    private fun canonical(text: String): String {
        if (!text.contains('_') && text.none { it.isUpperCase() }) return text
        val term = Term.parse(text, TermValues.operators)
        val names =
            term.variables
                .distinct()
                .mapIndexed { i, v -> v to Atom.of("_V$i") }
                .toMap()
        return term.apply(Substitution.of(names)).toString()
    }

    /** [explanation], a set of arguments, is no longer in the grounded extension once [intervention] is added */
    private fun breaks(
        request: Solve.Request<ExecutionContext>,
        explanation: Set<Argument>,
        intervention: PlList,
        effect: Term,
    ): Boolean {
        val reading = graphOf(request, intervened(request, intervention), effect)
        val inKeys =
            reading.graph.labellings
                .filter { it.label == "in" }
                .map { keyOf(it.argument) }
                .toHashSet()
        return !explanation.all { keyOf(it) in inKeys }
    }

    /**
     * The arguments IN or UND attacking a member of [explanation] once [intervention] is added (by their keys, see keyOf):
     * the attackers of the revised condition of a cause
     */
    private fun attackers(
        request: Solve.Request<ExecutionContext>,
        explanation: Set<Argument>,
        intervention: PlList,
        effect: Term,
    ): Set<List<Any>> {
        val reading = graphOf(request, intervened(request, intervention), effect)
        val keys = explanation.map { keyOf(it) }.toHashSet()
        return reading.graph.labellings
            .filter { keyOf(it.argument) in keys }
            .flatMap { member -> reading.attackers(member.argument).filter { it.label == "in" || it.label == "und" } }
            .map { keyOf(it.argument) }
            .toSet()
    }

    // The explanations of the arguments for effect IN in the grounded extension of the theory
    private fun explanationsOf(
        request: Solve.Request<ExecutionContext>,
        effect: Term,
    ): List<Set<Argument>> {
        val base = graphOf(request, theory(request), effect)
        return base.graph.labellings
            .filter { it.label == "in" && concludes(it.argument, effect) }
            .flatMap { explanations(base, it.argument) }
    }

    // The NESS condition of [intervention] for [explanation]. Original (Def. 4.3 of the ICAIL 2025 paper): it removes
    // the explanation from the grounded extension, and no proper part of it does. Revised: it gives an argument IN or UND
    // attacking a member of the explanation that no proper part of it gives (minimal for the attacker it builds)
    private fun nessFor(
        request: Solve.Request<ExecutionContext>,
        explanation: Set<Argument>,
        intervention: PlList,
        effect: Term,
        revised: Boolean,
    ): Boolean {
        val parts = properSubsets(request, intervention)
        if (!revised) {
            return breaks(request, explanation, intervention, effect) &&
                parts.none { breaks(request, explanation, it, effect) }
        }
        val built = attackers(request, explanation, intervention, effect)
        if (built.isEmpty()) return false
        val byParts = parts.flatMap { attackers(request, explanation, it, effect) }.toSet()
        return built.any { it !in byParts }
    }

    /**
     * Cause is a NESS cause of Effect: for some explanation of an argument for [effect] in the grounded extension (a
     * subset-minimal strongly admissible set containing it, Def. 3.3 of the ICAIL 2025 paper), some intervention
     * containing the complement of [cause] (among those the module generates) meets the NESS condition for it (see
     * nessFor), original or [revised]; revised, an argument for the cause is IN as well. The intervention, if any
     */
    fun ness(
        request: Solve.Request<ExecutionContext>,
        cause: Term,
        effect: Term,
        revised: Boolean,
    ): PlList? {
        if (revised && !isAccepted(request, cause)) return null
        val candidates = interventions(request, cause, effect).toList()
        return explanationsOf(request, effect).firstNotNullOfOrNull { explanation ->
            candidates.firstOrNull { nessFor(request, explanation, it, effect, revised) }
        }
    }

    /** [intervention] meets the NESS condition for [effect], for some explanation of it (see ness) */
    fun nessIntervention(
        request: Solve.Request<ExecutionContext>,
        intervention: PlList,
        effect: Term,
        revised: Boolean,
    ): Boolean = explanationsOf(request, effect).any { nessFor(request, it, intervention, effect, revised) }

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
