package it.unibo.tuprolog.argumentation.core.runtime

import it.unibo.tuprolog.core.Struct
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
