package it.unibo.tuprolog.argumentation.core.libraries.evaluation.pipeline.flags

import it.unibo.tuprolog.argumentation.Semantics
import it.unibo.tuprolog.argumentation.core.libraries.ArgsFlag

object ArgumentLabellingMode : ArgsFlag {
    override fun predicate(): String = "argumentLabellingMode"

    override fun default(): String = Semantics.GROUNDED.value

    override val isOpen: Boolean = true

    override fun values(): Iterable<String> = Semantics.entries.map { it.value }
}
