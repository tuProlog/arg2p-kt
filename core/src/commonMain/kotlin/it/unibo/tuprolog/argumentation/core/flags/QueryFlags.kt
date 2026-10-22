package it.unibo.tuprolog.argumentation.core.flags

import it.unibo.tuprolog.argumentation.core.libs.ArgsFlag

object QueryMode : ArgsFlag<Boolean, Unit> {
    override fun predicate(): String = "queryMode"

    override val isSwitch: Boolean = true

    override fun default(): Boolean = true

    override fun values() {}
}

object AmbiguityBlocking : ArgsFlag<Boolean, Unit> {
    override fun predicate(): String = "ambiguityBlocking"

    override val isSwitch: Boolean = true

    override fun default(): Boolean = true

    override fun values() {}
}
