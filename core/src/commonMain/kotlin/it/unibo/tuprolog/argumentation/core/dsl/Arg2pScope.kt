package it.unibo.tuprolog.argumentation.core.dsl

import it.unibo.tuprolog.dsl.theory.LogicProgrammingScopeWithTheories

interface PrologWithArgumentation : LogicProgrammingScopeWithTheories {
    infix fun Any.call(other: Any) = structOf("::", this.toTerm(), other.toTerm())
}

internal class PrologWithArgumentationImpl :
    PrologWithArgumentation, LogicProgrammingScopeWithTheories by LogicProgrammingScopeWithTheories.of()

fun <R> arg2pScope(function: PrologWithArgumentation.() -> R): R = PrologWithArgumentationImpl().function()
