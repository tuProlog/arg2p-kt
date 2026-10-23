package it.unibo.tuprolog.argumentation.core.engine.fsm

import it.unibo.tuprolog.argumentation.core.engine.PrologExecutionContext
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.TimeInstant

/**
 * A predicate the engine solves natively (arg2p), as its clauses in the libraries would, but without selecting them
 * and solving their goals one by one: `StateRuleSelection` hands the goals of its [signature] to it.
 */
internal interface ControlConstruct {
    val signature: Signature

    /** The state following the selection of [goal], the current goal of [context], at the instant [time] */
    fun select(
        context: PrologExecutionContext,
        goal: Struct,
        time: TimeInstant,
    ): State
}

/** The control constructs of the engine, looked up by the signature of a goal */
internal object ControlConstructs {
    private val constructs: List<ControlConstruct> = listOf(CallConstruct, NegationConstruct)

    private val byFunctor: Map<String, List<ControlConstruct>> = constructs.groupBy { it.signature.name }

    /** The construct solving [goal], if any */
    fun of(goal: Struct): ControlConstruct? = byFunctor[goal.functor]?.firstOrNull { it.signature.arity == goal.arity }
}
