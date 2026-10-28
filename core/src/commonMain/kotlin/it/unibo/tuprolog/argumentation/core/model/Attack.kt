@file:Suppress("NON_EXPORTABLE_TYPE")

package it.unibo.tuprolog.argumentation.core.model

import it.unibo.tuprolog.argumentation.core.model.terms.AttackTerm
import it.unibo.tuprolog.core.Atom
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.parsing.parse
import kotlin.js.JsExport

/**
 * An attack of a graph: [attacker] attacks [target], with its [type] (rebut, undermine, undercut, ...) and the
 * sub-argument of the target it hits ([on]), when known
 */
@JsExport
data class Attack(
    val attacker: Argument,
    val target: Argument,
    val type: AttackType? = null,
    val on: Argument? = null,
) {
    override fun toString(): String = toTerm().toString()

    /** The term of the attack (see [AttackTerm]), none for the type or the sub-argument not known */
    @JsExport.Ignore
    fun toTerm(): Struct =
        AttackTerm.of(
            type?.let { Term.parse(it) } ?: Atom.of("none"),
            attacker.termRepresentation(),
            target.termRepresentation(),
            on?.termRepresentation() ?: Atom.of("none"),
        )
}
