package it.unibo.tuprolog.argumentation.core.dsl

import it.unibo.tuprolog.argumentation.core.engine.Arg2pPrologSolverFactory
import it.unibo.tuprolog.dsl.solve.LogicProgrammingScope

interface PrologWithArgumentation : LogicProgrammingScope {
    infix fun Any.call(other: Any) = structOf("::", this.toTerm(), other.toTerm())
}

internal class PrologWithArgumentationImpl :
    PrologWithArgumentation,
    LogicProgrammingScope by LogicProgrammingScope.of(Arg2pPrologSolverFactory)

fun <R> arg2pScope(function: PrologWithArgumentation.() -> R): R = PrologWithArgumentationImpl().function()
