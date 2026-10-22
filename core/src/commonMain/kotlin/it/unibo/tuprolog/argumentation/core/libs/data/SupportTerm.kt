package it.unibo.tuprolog.argumentation.core.libs.data

import it.unibo.tuprolog.argumentation.core.libs.PrologModule
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term

/**
 * The term of a support (module `support`): `support(Supporter, Supported)`, Supporter being a sub-argument of
 * Supported. Kotlin reads and builds it through [parts] and [of].
 */
object SupportTerm : PrologModule("prolog.argumentation.data.support", "support") {
    override val prologRawTheory: String =
        """
        %! new(?Supporter, ?Supported, ?Support)
        %  Support is made of its parts.
        new(Supporter, Supported, support(Supporter, Supported)).

        %! supporter(?Support, ?Supporter)
        supporter(support(Supporter, _), Supporter).

        %! supported(?Support, ?Supported)
        supported(support(_, Supported), Supported).
        """.trimIndent()

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
