package it.unibo.tuprolog.argumentation.core.libs.graph

import it.unibo.tuprolog.argumentation.core.dsl.arg2pScope
import it.unibo.tuprolog.argumentation.core.libs.basic.Arg2pUnificator
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.exception.error.TypeError
import it.unibo.tuprolog.solve.primitive.Solve
import it.unibo.tuprolog.core.List as LogicList

// Argumentation graph and labels of the active context, read by native primitives

/** Instances of goal in the given branch (the active one by default), read with a single context_all call */
internal fun Solve.Request<ExecutionContext>.contextInstances(
    goal: Struct,
    branch: Term? = null,
): List<Struct> {
    val request = this
    // request.solve explicitly: inside arg2pScope a bare solve would resolve to the scope's own solver
    return arg2pScope {
        val instances = varOf("Instances")
        request
            .solve(if (branch == null) "context_all"(goal, instances) else "context_all"(branch, goal, instances))
            .firstOrNull { it.isYes }
            ?.substitution
            ?.get(instances)
            ?.castToList()
            ?.toList()
            ?.map { it.castToStruct() } ?: emptyList()
    }
}

/** Identifiers of the arguments in the graph */
internal fun Solve.Request<ExecutionContext>.argumentIds(): List<Term> =
    arg2pScope { contextInstances("clause"("arg"(`_`), `_`)).map { it[0].castToStruct()[0] } }

/** Attacks as (attacker, attacked) argument identifiers */
internal fun Solve.Request<ExecutionContext>.attacks(): List<Pair<Term, Term>> =
    arg2pScope {
        contextInstances("clause"("att"(`_`, `_`), `_`)).map { it[0].castToStruct().let { att -> att[0] to att[1] } }
    }

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
