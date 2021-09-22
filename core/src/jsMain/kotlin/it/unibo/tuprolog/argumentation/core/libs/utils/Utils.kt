package it.unibo.tuprolog.argumentation.core.libs.utils

import it.unibo.tuprolog.theory.Theory
import it.unibo.tuprolog.theory.parsing.parse

actual object Utils: UtilsBase() {
    override val prologRawTheory: String
        get() = it.unibo.tuprolog.argumentation.core.libs.sources.Utils.theoryCode
}
