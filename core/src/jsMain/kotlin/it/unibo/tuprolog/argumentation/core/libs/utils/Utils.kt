package it.unibo.tuprolog.argumentation.core.libs.utils

import it.unibo.tuprolog.theory.Theory
import it.unibo.tuprolog.theory.parsing.parse

actual object Utils: UtilsBase() {
    override val prologTheory: Theory
        get() = Theory.parse(it.unibo.tuprolog.argumentation.core.libs.sources.Utils.theoryCode)
}
