package it.unibo.tuprolog.argumentation.core.libs.graph.labelling

import it.unibo.tuprolog.argumentation.core.TheoryUtils
import it.unibo.tuprolog.theory.Theory

actual object CompleteLabeller: CompleteLabellerBase() {
    override val prologTheory: Theory
        get() = TheoryUtils.loadTheoryFromPrologFile("core/labellings/argument/complete")
}
