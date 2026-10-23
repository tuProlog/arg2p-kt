package it.unibo.tuprolog.argumentation.core.modules.data

import it.unibo.tuprolog.argumentation.core.modules.PrologModule

/**
 * The term of a support (module `support`): `support(Supporter, Supported)`, Supporter being a sub-argument of
 * Supported. Kotlin reads and builds it through [parts] and [of].
 */
object SupportModule : PrologModule("prolog.argumentation.data.support", "support") {
    override val prologRawTheory: String =
        // language=Prolog
        """
        %! new(?Supporter, ?Supported, ?Support)
        %  Support is made of its parts.
        new(Supporter, Supported, support(Supporter, Supported)).

        %! supporter(?Support, ?Supporter)
        supporter(support(Supporter, _), Supporter).

        %! supported(?Support, ?Supported)
        supported(support(_, Supported), Supported).
        """.trimIndent()
}
