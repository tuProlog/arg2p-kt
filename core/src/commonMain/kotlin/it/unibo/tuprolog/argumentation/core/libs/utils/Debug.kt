package it.unibo.tuprolog.argumentation.core.libs.utils

import it.unibo.tuprolog.argumentation.core.libs.ArgsFlag
import it.unibo.tuprolog.argumentation.core.libs.PrologModule

abstract class DebugBase : PrologModule("prolog.argumentation.debug", "debug") {
    override val baseFlags: Iterable<ArgsFlag<*, *>>
        get() = listOf(DebugOutput)
}

expect object Debug : DebugBase {
    override val prologRawTheory: String
}

// The debug output of the modules (writeDebug/1), off unless the theory states it (debug.)
object DebugOutput : ArgsFlag<Boolean, Unit> {
    override fun predicate(): String = "debug"

    override val isSwitch: Boolean = true

    override fun default(): Boolean = false

    override fun values() {}
}
