package it.unibo.tuprolog.argumentation.core.primitives.graph

import it.unibo.tuprolog.argumentation.core.dsl.arg2pScope
import it.unibo.tuprolog.argumentation.core.mining.arguments
import it.unibo.tuprolog.argumentation.core.mining.labels
import it.unibo.tuprolog.argumentation.core.modules.data.LabelTerm
import it.unibo.tuprolog.argumentation.core.primitives.graph.ConflictGraph
import it.unibo.tuprolog.argumentation.core.runtime.context.contextAssertAll
import it.unibo.tuprolog.argumentation.core.runtime.context.contextInstances
import it.unibo.tuprolog.argumentation.core.runtime.context.contextLibrary
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.primitive.Solve

// The graph kept in the active context and its labels, for the native primitives: the Kotlin side of the module
// graphStore, reading the same facts

/** The identifiers of the arguments of the graph */
internal fun Solve.Request<ExecutionContext>.argumentIds(): List<Term> =
    arg2pScope { contextInstances("clause"("arg"(`_`), `_`)).map { it[0].castToStruct()[0] } }

/** The arguments of the graph, as (identifier, argument) pairs */
internal fun Solve.Request<ExecutionContext>.storedArguments(): List<Pair<Term, Term>> =
    arg2pScope {
        contextInstances("clause"("arg"(`_`), "argument"(`_`)))
            .map { it[0].castToStruct()[0] to it[1].castToStruct()[0] }
    }

/** The attacks between the arguments of a branch, by argument identifier, with the attackers and the attacked of each */
internal class AttackGraph(
    /** The attacks, as (attacker, attacked) pairs, newest first */
    val attacks: List<Pair<Term, Term>>,
) {
    val attackers: Map<Term, List<Term>> by lazy(LazyThreadSafetyMode.NONE) { attacks.groupBy({ it.second }, { it.first }) }
    val attacked: Map<Term, List<Term>> by lazy(LazyThreadSafetyMode.NONE) { attacks.groupBy({ it.first }, { it.second }) }
}

/**
 * The attack graph of the active branch, from its att/2 clauses: built once until they change (the native
 * labellings read it at each step of their search)
 */
internal fun Solve.Request<ExecutionContext>.attackGraph(): AttackGraph =
    contextLibrary.derived("att", 2, "attackGraph") { clauses ->
        AttackGraph(clauses.map { it.head!!.let { att -> att[0] to att[1] } })
    }

/** Attacks as (attacker, attacked) argument identifiers */
internal fun Solve.Request<ExecutionContext>.attacks(): List<Pair<Term, Term>> = attackGraph().attacks

/** The conflicts between the arguments of the active branch (see [ConflictGraph]), built once until the attacks change */
internal fun Solve.Request<ExecutionContext>.conflictGraph(): ConflictGraph<Term> =
    contextLibrary.derived("att", 2, "conflictGraph") { ConflictGraph(attacks()) }

/** The identifiers of the arguments labelled with [label] (in, out or und), in the given or the active branch */
internal fun Solve.Request<ExecutionContext>.labelledIds(
    label: String,
    branch: Term? = null,
): Set<Term> = arg2pScope { contextInstances(LabelTerm.identifier(label, `_`), branch).map { it[0] }.toSet() }

/**
 * The arguments labelled, by themselves and by their identifiers, in a single assertion: for each label in turn, its
 * (identifier, argument) pairs
 */
internal fun Solve.Request<ExecutionContext>.labelArguments(labels: List<Pair<String, List<Pair<Term, Term>>>>) =
    contextAssertAll(
        labels.flatMap { (label, arguments) ->
            arguments.flatMap { (id, argument) -> listOf(LabelTerm.argument(label, argument), LabelTerm.identifier(label, id)) }
        },
    )
