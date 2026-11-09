package it.unibo.tuprolog.argumentation.core.libraries.language.parser.helpers

import it.unibo.tuprolog.core.Clause
import it.unibo.tuprolog.core.List
import it.unibo.tuprolog.core.Struct

/** The term of the module `rule` as Kotlin reads and builds it */
object RuleTerm {
    /** The clause of a rule or premise from the list in its body, [Id, Premises, Conclusion] or [Id, Conclusion] */
    fun of(parts: List): Clause? {
        val items = parts.toList()
        return when (items.size) {
            2, 3 -> Clause.of(Struct.of(HEAD, items.last()), parts)
            else -> null
        }
    }

    const val HEAD = "rl"
}
