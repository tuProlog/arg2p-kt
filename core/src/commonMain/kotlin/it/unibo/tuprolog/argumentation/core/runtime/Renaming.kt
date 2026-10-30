package it.unibo.tuprolog.argumentation.core.runtime

import it.unibo.tuprolog.core.Substitution
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.Var

/**
 * A copy of this term with fresh variables, one for each of its variables told apart by identity. [Term.freshCopy]
 * tells them apart by name: it takes two variables with the same name for one (as the renamings of a clause make
 * them, each keeping the names of the clause) and each occurrence of an anonymous variable for a variable of its own,
 * so a term the engine built would be copied with places joined or parted.
 */
@Suppress("UNCHECKED_CAST")
fun <T : Term> T.renamedCopy(): T {
    if (isGround) return this
    val fresh = LinkedHashMap<Var, Term>()
    for (variable in variables) {
        if (variable !in fresh) fresh[variable] = Var.of(if (variable.isAnonymous) "_R" else variable.name)
    }
    return apply(Substitution.unifier(fresh)) as T
}
