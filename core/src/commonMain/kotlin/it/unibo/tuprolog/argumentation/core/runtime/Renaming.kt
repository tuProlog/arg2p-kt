package it.unibo.tuprolog.argumentation.core.runtime

import it.unibo.tuprolog.argumentation.InternalArg2pApi
import it.unibo.tuprolog.core.Atom
import it.unibo.tuprolog.core.Integer
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Substitution
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.Var

/**
 * A copy of this term with fresh variables, one for each of its variables told apart by identity. [Term.freshCopy]
 * tells them apart by name: it takes two variables with the same name for one (as the renamings of a clause make
 * them, each keeping the names of the clause) and each occurrence of an anonymous variable for a variable of its own,
 * so a term the engine built would be copied with places joined or parted. The copy is a single visit of the term,
 * its ground parts shared, as cheap as [Term.freshCopy].
 */
@Suppress("UNCHECKED_CAST")
fun <T : Term> T.renamedCopy(): T = if (isGround) this else renamed(this, HashMap()) as T

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

private fun renamed(
    term: Term,
    fresh: MutableMap<Var, Var>,
): Term =
    when {
        term.isGround -> term
        term is Var -> fresh.getOrPut(term) { Var.of(if (term.isAnonymous) "_R" else term.name) }
        term is Struct -> Struct.of(term.functor, List(term.arity) { renamed(term[it], fresh) })
        else -> term
    }
