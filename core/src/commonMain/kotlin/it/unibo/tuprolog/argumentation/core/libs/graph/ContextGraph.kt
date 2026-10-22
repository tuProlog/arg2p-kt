package it.unibo.tuprolog.argumentation.core.libs.graph

import it.unibo.tuprolog.argumentation.core.dsl.arg2pScope
import it.unibo.tuprolog.argumentation.core.libs.basic.Arg2pUnificator
import it.unibo.tuprolog.argumentation.core.libs.basic.Context
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.exception.error.ExistenceError
import it.unibo.tuprolog.solve.exception.error.TypeError
import it.unibo.tuprolog.solve.primitive.Solve
import it.unibo.tuprolog.core.List as LogicList

// Argumentation graph and labels of the active context, read by native primitives

// The context library of the solver, every arg2p solver has one (an existence error for context_all/2 otherwise)
private val Solve.Request<ExecutionContext>.contextLibrary: Context
    get() = Context.of(context) ?: throw ExistenceError.forProcedure(context, Signature("context_all", 2))

/** Instances of goal in the given branch (the active one by default, none for a branch that does not exist) */
internal fun Solve.Request<ExecutionContext>.contextInstances(
    goal: Struct,
    branch: Term? = null,
): List<Struct> {
    val index = branch?.let { it.asInteger()?.intValue?.toInt() ?: return emptyList() }
    return contextLibrary.instances(goal, index).map { it.castToStruct() }
}

/** A new branch of the given one, made active (context_branch/2): false if there is no such branch */
internal fun Solve.Request<ExecutionContext>.contextBranch(from: Term): Boolean {
    val branch = from.asInteger()?.intValue?.toInt() ?: throw TypeError.forGoal(context, signature, TypeError.Expected.INTEGER, from)
    return contextLibrary.branch(branch) != null
}

/** The terms asserted in the active branch, in order (context_assert_all/1) */
internal fun Solve.Request<ExecutionContext>.contextAssertAll(terms: List<Term>) = contextLibrary.assertAll(terms)

/** Identifiers of the arguments in the graph */
internal fun Solve.Request<ExecutionContext>.argumentIds(): List<Term> =
    arg2pScope { contextInstances("clause"("arg"(`_`), `_`)).map { it[0].castToStruct()[0] } }

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

/** Identifiers of the arguments with the given label fact (inId, outId, undId), in the given or active branch */
internal fun Solve.Request<ExecutionContext>.labelled(
    label: String,
    branch: Term? = null,
): Set<Term> = arg2pScope { contextInstances(label(`_`), branch).map { it[0] }.toSet() }

// Arguments and replies of the native primitives

/** Identifiers in the list argument at the given index */
internal fun Solve.Request<ExecutionContext>.ids(index: Int): List<Term> {
    val ids = arguments[index]
    if (ids !is LogicList) throw TypeError.forGoal(context, signature, TypeError.Expected.LIST, ids)
    return ids.toList()
}

/** A reply binding the argument at the given index to the list of values, if they unify */
internal fun Solve.Request<ExecutionContext>.replyList(
    index: Int,
    values: List<Term>,
): Solve.Response? =
    Arg2pUnificator
        .mgu(arguments[index], LogicList.of(values))
        .takeIf { it.isSuccess }
        ?.let { replyWith(it) }
