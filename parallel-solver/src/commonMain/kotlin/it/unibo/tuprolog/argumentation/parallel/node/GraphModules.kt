package it.unibo.tuprolog.argumentation.parallel.node

import it.unibo.tuprolog.argumentation.core.engine.AbstractNativePrimitive
import it.unibo.tuprolog.argumentation.core.engine.NativeCall
import it.unibo.tuprolog.argumentation.core.engine.items
import it.unibo.tuprolog.argumentation.core.engine.replyWord
import it.unibo.tuprolog.argumentation.core.modules.Loadable
import it.unibo.tuprolog.argumentation.core.modules.ModuleRegistry
import it.unibo.tuprolog.argumentation.core.modules.PrologModule
import it.unibo.tuprolog.argumentation.core.primitives.graph.saveArgumentRecords
import it.unibo.tuprolog.argumentation.core.primitives.graph.saveBuiltArguments
import it.unibo.tuprolog.argumentation.parallel.message.SharedArgument
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.primitive.Primitive
import it.unibo.tuprolog.core.List as PlList

/** The graph the workers of a catalog built, as the graph store saves it, for the modules of its solver */
internal interface BuiltGraph {
    /**
     * The arguments that [need] asks for (`whole`, the whole graph, or `explore(Statement)`, in query mode the arguments the
     * labels of the arguments for Statement depend on), as terms: null if the workers did not build them, which the catalog has
     * them do
     */
    fun arguments(need: Term): Collection<SharedArgument>?

    /** The identifiers of the arguments in the solver, as saved, in the order of [arguments] */
    fun identified(ids: IntArray)

    /** The supports between the arguments identified: of each supporter, the argument supported */
    fun supports(): List<Pair<Int, Int>>

    /** The supports not saved as arguments (with constraints), as a list of support/2 terms */
    fun boundSupports(): Term

    /** The direct attacks the workers found between the arguments identified: the type, the attacker, the attacked */
    fun attacks(): List<Triple<String, Int, Int>>
}

/**
 * The modules of the solver of a catalog: those of the standard solver, but for the graph builder of the rules (the
 * module ruleGraph) and query mode (queryExploration), which save the arguments and the attacks the workers built
 * ([graph]) rather than build them
 */
internal fun graphModules(graph: BuiltGraph): List<Loadable> {
    val modules = listOf(RuleGraph(graph), QueryExploration)
    val replaced = modules.map { it.identifier() }.toSet()
    return modules + ModuleRegistry.all.filterNot { it.identifier() in replaced }
}

// The graph builder of the rules: the arguments the workers built (a graph not built yet stops the resolution, see
// built/1), and the attacks, those the workers found as the module attackBuilder does, which the graph store extends to
// the arguments built on the attacked ones (the arguments derived from them are added when the graph is changed, see
// structuredWhole::modifyGraph/0)
private class RuleGraph(
    private val graph: BuiltGraph,
) : PrologModule("prolog.argumentation.parallel.graph", "ruleGraph") {
    override val primitives: Map<Signature, Primitive>
        get() = listOf(Arguments(), SaveBuilt(), Identifiers(), Supports(), Attacks()).associateBy { it.signature }

    override val prologRawTheory: String =
        // language=Prolog
        """
        %! buildArgumentationGraph
        %  The arguments the workers built and the attacks between them.
        buildArgumentationGraph :-
            buildArguments,
            buildAttacks.

        %! buildArguments
        %  The arguments of the theory, built by the workers.
        buildArguments :-
            built(whole).

        %! buildAttacks
        %  The attacks between the saved arguments: the direct ones the workers found, extended by the graph store.
        buildAttacks :-
            builtAttacks(Attacks),
            graphStore::saveAttackIds(Attacks).

        %! built(+Need)
        %  The arguments Need asks for (whole, or explore(Statement)) and their supports, saved as the workers built
        %  them; if they are not built yet, the resolution stops, to be started again once they are.
        built(Need) :-
            savedArguments(Need), !,
            builtSupports(Supports, Bound),
            graphStore::saveSupportIds(Supports),
            graphStore::saveSupports(Bound).
        built(Need) :-
            throw(not_built(Need)).

        % The arguments saved: straight in the store, unless they may have variables (with constraints), where the graph store
        % tells whether a more general one is saved
        savedArguments(Need) :-
            parser::constrained, !,
            builtArguments(Need, Arguments),
            graphStore::saveArguments(a, Arguments),
            findall(Id, (member(A, Arguments), graphStore::argumentId(A, Id)), Ids),
            builtIdentifiers(Ids).
        savedArguments(Need) :-
            saveBuilt(Need).
        """.trimIndent()

    // builtArguments(+Need, -Arguments)
    inner class Arguments : AbstractNativePrimitive(Signature("builtArguments", 2)) {
        override fun call(call: NativeCall): Int {
            val arguments = graph.arguments(call.term(call.arg(0))) ?: return NativeCall.FAIL
            return call.replyWord(1, call.word(PlList.of(arguments.map { it.argument })))
        }
    }

    // saveBuilt(+Need): the arguments Need asks for saved, and known by their identifiers
    inner class SaveBuilt : AbstractNativePrimitive(Signature("saveBuilt", 1)) {
        override fun call(call: NativeCall): Int {
            val arguments = graph.arguments(call.term(call.arg(0))) ?: return NativeCall.FAIL
            graph.identified(call.saveShared(arguments.toList()))
            return NativeCall.CONTINUE
        }
    }

    // builtIdentifiers(+Ids): the identifiers of the arguments, as saved
    inner class Identifiers : AbstractNativePrimitive(Signature("builtIdentifiers", 1)) {
        override fun call(call: NativeCall): Int {
            val ids = call.items(call.arg(0))!!
            graph.identified(IntArray(ids.size) { call.intValue(call.deref(ids[it])).toInt() })
            return NativeCall.CONTINUE
        }
    }

    // builtSupports(-Supports, -Bound): those between the arguments, s(Supporter, Supported), and the others
    inner class Supports : AbstractNativePrimitive(Signature("builtSupports", 2)) {
        override fun call(call: NativeCall): Int {
            val supports = graph.supports()
            val list =
                call.list(
                    IntArray(supports.size) { call.struct("s", intArrayOf(call.int(supports[it].first), call.int(supports[it].second))) },
                )
            return if (call.unify(call.arg(0), list)) call.replyWord(1, call.word(graph.boundSupports())) else NativeCall.FAIL
        }
    }

    // builtAttacks(-Attacks): each a(Type, Attacker, Attacked), by their identifiers
    inner class Attacks : AbstractNativePrimitive(Signature("builtAttacks", 1)) {
        override fun call(call: NativeCall): Int {
            val attacks = graph.attacks()
            return call.replyWord(
                0,
                call.list(
                    IntArray(attacks.size) {
                        val (type, attacker, attacked) = attacks[it]
                        call.struct("a", intArrayOf(call.atom(type), call.int(attacker), call.int(attacked)))
                    },
                ),
            )
        }
    }
}

// [arguments] saved, in their order: each run of them from the same batch of records straight from the records, those
// given as terms (a message read as text) from the terms
private fun NativeCall.saveShared(arguments: List<SharedArgument>): IntArray {
    val ids = IntArray(arguments.size)
    var start = 0
    while (start < arguments.size) {
        val batch = arguments[start].batch
        var end = start + 1
        while (end < arguments.size && arguments[end].batch === batch) end++
        val run = arguments.subList(start, end)
        val saved =
            if (batch == null) {
                saveBuiltArguments("a", run.map { it.argument })
            } else {
                saveArgumentRecords("a", batch, IntArray(run.size) { run[it].index })
            }
        saved.copyInto(ids, start)
        start = end
    }
    return ids
}

// Query mode: the arguments the workers built exploring the query, as the module queryExploration explores it on its
// rules, and expanded as the locality of the semantics goes
private object QueryExploration : PrologModule("prolog.argumentation.parallel.exploration", "queryExploration") {
    override val prologRawTheory: String =
        // language=Prolog
        """
        %! explore(+Query, +Locality)
        %  The arguments for Query and those their labels may depend on, built by the workers, who expand and extend
        %  their exploration as Locality goes.
        explore(Query, _) :-
            ruleGraph::built(explore(Query)).
        """.trimIndent()
}
