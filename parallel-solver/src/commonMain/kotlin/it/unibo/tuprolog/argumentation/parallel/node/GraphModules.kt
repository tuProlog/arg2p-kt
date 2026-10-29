package it.unibo.tuprolog.argumentation.parallel.node

import it.unibo.tuprolog.argumentation.core.modules.ArgLibrary
import it.unibo.tuprolog.argumentation.core.modules.Loadable
import it.unibo.tuprolog.argumentation.core.modules.ModuleRegistry
import it.unibo.tuprolog.argumentation.core.modules.PrologModule
import it.unibo.tuprolog.argumentation.core.runtime.Arg2pUnificator
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Substitution
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.primitive.Primitive
import it.unibo.tuprolog.solve.primitive.Solve
import it.unibo.tuprolog.solve.primitive.TernaryRelation
import it.unibo.tuprolog.solve.primitive.UnaryPredicate

/** The graph the workers of a catalog built, as the graph store saves it, for the modules of its solver */
internal interface BuiltGraph {
    /**
     * The arguments and their supports that [need] asks for (`whole`, the whole graph, or `explore(Statement)`, in query
     * mode the arguments the labels of the arguments for Statement depend on), as lists of argument terms and of
     * support/2 terms: null if the workers did not build them, which the catalog has them do
     */
    fun arguments(need: Term): Pair<Term, Term>?

    /** The direct attacks the workers found, as a list of attack/4 terms: none if they find none (with metaConflicts) */
    val attacks: Term?
}

/**
 * The modules of the solver of a catalog: those of the standard solver, but for the graph builder of the rules (the
 * module ruleGraph) and query mode (queryExploration), which save the arguments and the attacks the workers built
 * ([graph]) rather than build them
 */
internal fun graphModules(graph: BuiltGraph): List<ArgLibrary> {
    val modules = listOf(RuleGraph(graph), QueryExploration)
    val replaced = modules.map { it.identifier() }.toSet()
    return modules + ModuleRegistry.all.filterNot { (it as? Loadable)?.identifier() in replaced }
}

// The graph builder of the rules: the arguments the workers built (a graph not built yet stops the resolution, see
// built/1), those derived from them (conflictArguments), and the attacks, those the workers found, which the graph
// store extends to the arguments built on the attacked ones, or all of them, by the module attackBuilder, if they
// find none
private class RuleGraph(
    private val graph: BuiltGraph,
) : PrologModule("prolog.argumentation.parallel.graph", "ruleGraph") {
    override val primitives: Map<Signature, Primitive>
        get() = listOf(Arguments(), Attacks()).associate { it.descriptionPair }

    override val prologRawTheory: String =
        // language=Prolog
        """
        %! buildArgumentationGraph
        %  The arguments the workers built, those derived from them, and the attacks between them.
        buildArgumentationGraph :-
            buildArguments,
            buildAdditionalArguments,
            buildAttacks.

        %! buildArguments
        %  The arguments of the theory, built by the workers.
        buildArguments :-
            built(whole).

        %! buildAdditionalArguments
        %  The arguments derived from the others (see the module conflictArguments).
        buildAdditionalArguments :-
            conflictArguments::buildAdditionalArguments.

        %! buildAttacks
        %  The attacks between the saved arguments: the direct ones the workers found, extended by the graph store.
        buildAttacks :-
            builtAttacks(Attacks), !,
            graphStore::saveAttacks(Attacks).
        buildAttacks :-
            attackBuilder::buildAttacks.

        %! built(+Need)
        %  The arguments Need asks for (whole, or explore(Statement)) and their supports, saved as the workers built
        %  them; if they are not built yet, the resolution stops, to be started again once they are.
        built(Need) :-
            builtArguments(Need, Arguments, Supports), !,
            graphStore::saveArguments(a, Arguments),
            graphStore::saveSupports(Supports).
        built(Need) :-
            throw(not_built(Need)).
        """.trimIndent()

    // builtArguments(+Need, -Arguments, -Supports)
    inner class Arguments : TernaryRelation.WithoutSideEffects<ExecutionContext>("builtArguments") {
        override fun Solve.Request<ExecutionContext>.computeAllSubstitutions(
            first: Term,
            second: Term,
            third: Term,
        ): Sequence<Substitution> =
            graph.arguments(first)?.let { (arguments, supports) ->
                unified(Struct.of("b", second, third), Struct.of("b", arguments, supports))
            }
                ?: emptySequence()
    }

    // builtAttacks(-Attacks)
    inner class Attacks : UnaryPredicate.WithoutSideEffects<ExecutionContext>("builtAttacks") {
        override fun Solve.Request<ExecutionContext>.computeAllSubstitutions(first: Term): Sequence<Substitution> =
            graph.attacks?.let { unified(first, it) } ?: emptySequence()
    }

    private fun unified(
        first: Term,
        second: Term,
    ) = Arg2pUnificator.mgu(first, second).let { if (it.isSuccess) sequenceOf(it) else emptySequence() }
}

// Query mode: the arguments the workers built exploring the query, as the module queryExploration explores it on its
// rules, and expanded as the locality of the semantics goes
private object QueryExploration : PrologModule("prolog.argumentation.parallel.exploration", "queryExploration") {
    override val prologRawTheory: String =
        // language=Prolog
        """
        %! explore(+Query)
        %  The arguments for Query and those their labels may depend on, built by the workers.
        explore(Query) :-
            ruleGraph::built(explore(Query)).

        %! expand(+Locality)
        %  Done by the workers, as they explore.
        expand(_).
        """.trimIndent()
}
