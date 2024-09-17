package it.unibo.tuprolog.argumentation.core.dsl

import it.unibo.tuprolog.dsl.solve.LogicProgrammingScope
import it.unibo.tuprolog.solve.Solver

interface PrologWithArgumentation : LogicProgrammingScope {
    infix fun Any.call(other: Any) = structOf("::", this.toTerm(), other.toTerm())
}

internal class PrologWithArgumentationImpl :
    PrologWithArgumentation, LogicProgrammingScope by LogicProgrammingScope.of(Solver.prolog)

fun <R> arg2pScope(function: PrologWithArgumentation.() -> R): R = PrologWithArgumentationImpl().function()
