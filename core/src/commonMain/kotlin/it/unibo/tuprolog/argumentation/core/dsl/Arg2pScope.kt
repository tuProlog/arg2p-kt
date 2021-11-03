package it.unibo.tuprolog.argumentation.core.dsl

import it.unibo.tuprolog.dsl.PrologScope
import it.unibo.tuprolog.dsl.theory.PrologScopeWithTheories
import it.unibo.tuprolog.unify.Unificator

interface PrologWithArgumentation : PrologScopeWithTheories {
    infix fun Any.call(other: Any) = structOf("::", this.toTerm(), other.toTerm())
}

internal class PrologWithArgumentationImpl :
    PrologWithArgumentation, PrologScope by PrologScope.empty(), Unificator by Unificator.default

fun <R> arg2pScope(function: PrologWithArgumentation.() -> R): R = PrologWithArgumentationImpl().function()
