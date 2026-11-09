package it.unibo.tuprolog.argumentation.engine.vm

import it.unibo.tuprolog.argumentation.engine.InternalArg2pApi
import it.unibo.tuprolog.solve.library.Runtime

/**
 * A primitive on a goal the machine holds with its arguments in the registers, no term of it built ([module]: the word of
 * the atom it comes with)
 */
@InternalArg2pApi
interface RegisterGoalPrimitive {
    fun callOnRegisters(
        call: MachineCall,
        module: Int,
    ): Int
}

/** What the machine adds to a [NativeCall] for the primitives of arg2p, past what a call on terms can do */
@InternalArg2pApi
interface MachineCall : NativeCall {
    /** What [compute] finds in [libraries], kept for them until another function is asked: one to be kept itself, not made per call */
    fun libraryValue(compute: (Runtime) -> Any?): Any?

    /** The key [record] of the term [w] would have, with no copy of it, valid for this call */
    fun recordKey(w: Int): RecordKey

    /** The program the words of this call are of */
    val recordProgram: Program

    /** The goal of a [RegisterGoalPrimitive] entered against [libraries], as [NativeCall.inline] */
    fun inlineOnRegisters(
        libraries: Runtime,
        theoryVisible: Boolean,
    ): Int

    /** Enters the goal [goal] resolved with the clauses of [store] for it: their heads unified in place */
    fun storeGoal(
        goal: Int,
        store: ClauseStore,
        wrap: GoalWrapper?,
    ): Int

    /** Answers `clause(Head, Body)`, for a callable [head]: the clauses of [store] whose head and body unify with them */
    fun storeClause(
        head: Int,
        body: Int,
        store: ClauseStore,
    ): Int

    /**
     * Whether a fact of [store] unifies with [goal] (or, with [body], whether a clause does with `clause(goal, body)`),
     * nothing left bound; null when a rule may answer, whose body is to be solved
     */
    fun storeHolds(
        goal: Int,
        body: Int?,
        store: ClauseStore,
    ): Boolean?

    /** The facts of [store] that unify with [goal], newest first, nothing left bound and the registers as they were */
    fun storeMatching(
        goal: Int,
        store: ClauseStore,
    ): List<ClauseStore.Stored>
}

/** The goals of a body wrapped as a native primitive asks (see the lookups of the stores) */
fun interface GoalWrapper {
    fun wrap(
        call: NativeCall,
        goal: Int,
    ): Int
}
