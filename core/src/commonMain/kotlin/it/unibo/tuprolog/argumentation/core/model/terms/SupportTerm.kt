package it.unibo.tuprolog.argumentation.core.model.terms

import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term

/** The term of a support: `support(Supporter, Supported)`, Supporter being a sub-argument of Supported */
object SupportTerm {
    /** The term of a support, made of its parts */
    fun of(
        supporter: Term,
        supported: Term,
    ): Struct = Struct.of(FUNCTOR, supporter, supported)

    private const val FUNCTOR = "support"
}
