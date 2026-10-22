package it.unibo.tuprolog.argumentation.core.flags

import it.unibo.tuprolog.argumentation.core.libs.ArgsFlag

// The debug output of the modules (writeDebug/1), off unless the theory states it (debug.)
object DebugOutput : ArgsFlag<Boolean, Unit> {
    override fun predicate(): String = "debug"

    override val isSwitch: Boolean = true

    override fun default(): Boolean = false

    override fun values() {}
}
