package it.unibo.tuprolog.argumentation.core.runtime

import it.unibo.tuprolog.argumentation.InternalArg2pApi
import it.unibo.tuprolog.argumentation.engine.term.Arg2pUnificator
import it.unibo.tuprolog.core.Atom
import it.unibo.tuprolog.core.Integer
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Substitution
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.solve.Signature

/**
 * This term with its variables numbered in the order they appear (`'$VAR'(0)`, ...): the same for two terms equal
 * up to the names of their variables, which is how the graph store identifies arguments and statements
 */
@InternalArg2pApi
fun Term.canonical(): Term =
    if (isGround) {
        this
    } else {
        apply(Substitution.of(variables.distinct().mapIndexed { i, variable -> variable to Struct.of("\$VAR", Integer.of(i)) }.toMap()))
    }

/**
 * This term with each of its variables replaced by an atom of its own (`$var_0`, ...): a term (with none of these
 * atoms) unifies with it if and only if it generalises this term
 */
@InternalArg2pApi
fun Term.withVariablesFixed(): Term =
    if (isGround) {
        this
    } else {
        apply(Substitution.of(variables.distinct().mapIndexed { i, variable -> variable to Atom.of("\$var_$i") }.toMap()))
    }

/**
 * Whether this term generalises [other] (subsumes_term/2): it unifies with [other], the variables of [other] taken as
 * constants (see [withVariablesFixed]); neither is bound
 */
@InternalArg2pApi
fun Term.subsumes(other: Term): Boolean = Arg2pUnificator.match(this, other.withVariablesFixed())

/**
 * The signature of a callable: built from its functor and arity alone, as extractSignature() builds the indicator
 * term of the callable (an atom, an integer and their lazy values) only to read them back
 */
internal val Struct.signatureOf: Signature get() = Signature(functor, arity)
