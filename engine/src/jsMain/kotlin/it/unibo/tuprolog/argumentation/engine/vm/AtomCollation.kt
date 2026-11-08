package it.unibo.tuprolog.argumentation.engine.vm

import it.unibo.tuprolog.core.compareStringsLocaleIndependently

internal actual fun compareAtomNames(
    first: String,
    second: String,
): Int = compareStringsLocaleIndependently(first, second)
