package it.unibo.tuprolog.argumentation.engine.vm

/** The order of the names of two atoms, as tuProlog's: one collator, not one per comparison */
internal expect fun compareAtomNames(
    first: String,
    second: String,
): Int
