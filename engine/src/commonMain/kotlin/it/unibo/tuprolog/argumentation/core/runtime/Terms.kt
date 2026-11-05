package it.unibo.tuprolog.argumentation.core.runtime

import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.Var
import it.unibo.tuprolog.solve.Signature

/**
 * The variables of a term, collected by a plain recursion (Term.variables nests a lazy sequence for each argument),
 * skipping its parts ground at hand (see [isKnownGround]: the goals are fresh terms, whose groundness a plain compound
 * term computed by sequences over its whole subterm)
 */
internal fun Term.collectVariables(into: MutableSet<Var>) {
    when (this) {
        is Var -> into.add(this)
        is Struct -> if (!isKnownGround()) for (i in 0 until arity) this[i].collectVariables(into)
        else -> Unit
    }
}

/**
 * The signature of a callable: built from its functor and arity alone, as extractSignature() builds the indicator
 * term of the callable (an atom, an integer and their lazy values) only to read them back
 */
internal val Struct.signatureOf: Signature get() = Signature(functor, arity)
