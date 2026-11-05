package it.unibo.tuprolog.argumentation.core.engine.vm

/** A clause compiled by the platform (see [ClauseCodegen]) */
internal interface CompiledCode {
    /** Runs it on [machine] from the entry [entry]: the address to go on at, -1 if it failed */
    fun run(
        machine: Machine,
        entry: Int,
    ): Int
}

/** A clause compiled, and the addresses of its code each entry stands for */
internal class GeneratedClause(
    val unit: CompiledCode,
    val entries: IntArray,
)

/** Compiles clauses where the platform can */
internal expect object ClauseCodegen {
    /** The clause at [start] of [code] compiled; null if it cannot be */
    fun generate(
        code: IntArray,
        start: Int,
    ): GeneratedClause?
}
