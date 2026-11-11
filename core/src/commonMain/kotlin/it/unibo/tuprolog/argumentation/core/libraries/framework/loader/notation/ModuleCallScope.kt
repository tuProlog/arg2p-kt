package it.unibo.tuprolog.argumentation.core.libraries.framework.loader.notation

import it.unibo.tuprolog.dsl.solve.LogicProgrammingScope

/** The module calls in the terms a scope builds: `Module call Goal`, as `Module::Goal` in Prolog */
interface ModuleCallScope : LogicProgrammingScope {
    infix fun Any.call(other: Any) = structOf("::", this.toTerm(), other.toTerm())
}
