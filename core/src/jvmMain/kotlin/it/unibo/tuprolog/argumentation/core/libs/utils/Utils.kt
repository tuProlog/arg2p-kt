package it.unibo.tuprolog.argumentation.core.libs.utils

import it.unibo.tuprolog.argumentation.core.TheoryUtils
import it.unibo.tuprolog.theory.Theory

actual object Utils: UtilsBase() {
    override val prologRawTheory: String
        get() = TheoryUtils.loadTheoryFromPrologFile("utils/utils")
}