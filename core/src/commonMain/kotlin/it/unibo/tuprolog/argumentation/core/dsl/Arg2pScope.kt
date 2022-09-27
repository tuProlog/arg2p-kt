package it.unibo.tuprolog.argumentation.core.dsl

import it.unibo.tuprolog.dsl.LogicProgrammingScope
import it.unibo.tuprolog.dsl.theory.LogicProgrammingScopeWithTheories
import it.unibo.tuprolog.unify.Unificator

interface PrologWithArgumentation : LogicProgrammingScopeWithTheories {
    infix fun Any.call(other: Any) = structOf("::", this.toTerm(), other.toTerm())
}

internal class PrologWithArgumentationImpl :
    PrologWithArgumentation, LogicProgrammingScope by LogicProgrammingScope.empty(), Unificator by Unificator.default

fun <R> arg2pScope(function: PrologWithArgumentation.() -> R): R = PrologWithArgumentationImpl().function()
