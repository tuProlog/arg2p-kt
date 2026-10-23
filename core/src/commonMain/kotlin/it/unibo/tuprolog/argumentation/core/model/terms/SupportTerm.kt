package it.unibo.tuprolog.argumentation.core.model.terms

import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term

/** The term of the module SupportModule as Kotlin reads and builds it (see [SupportModule]) */
object SupportTerm {
    /** The parts of the term of a support, by name */
    class Parts(
        val supporter: Term,
        val supported: Term,
    )

    /** The parts of [support], the term of a support; null if it is not one */
    fun parts(support: Term): Parts? =
        (support as? Struct)
            ?.takeIf { it.functor == FUNCTOR && it.arity == 2 }
            ?.let { Parts(it[0], it[1]) }

    /** The term of a support, made of its parts */
    fun of(
        supporter: Term,
        supported: Term,
    ): Struct = Struct.of(FUNCTOR, supporter, supported)

    private const val FUNCTOR = "support"
}
