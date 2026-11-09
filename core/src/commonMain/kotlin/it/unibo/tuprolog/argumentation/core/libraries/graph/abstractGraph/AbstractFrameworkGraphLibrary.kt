package it.unibo.tuprolog.argumentation.core.libraries.graph.abstractGraph

import it.unibo.tuprolog.argumentation.core.libraries.graph.abstractGraph.prolog.THEORY
import it.unibo.tuprolog.argumentation.core.libraries.language.abstractLanguage.AbstractNotationLibrary

/**
 * The argumentation graph of an abstract theory (module `abstractGraph`, the graphBuilder of the profile abstract):
 * the arguments it states, `a.` or arg(a), and the attacks between them, `a -> b.` or att(a, b), and `a <-> b.` for a
 * mutual attack, facts or derived by Prolog clauses (`X -> Y :- beats(X, Y).`); an argument attacking or attacked is
 * an argument as well. Each is saved as abstract::solve saves them.
 */
internal object AbstractFrameworkGraphLibrary : AbstractNotationLibrary("abstractGraph") {
    override val prologRawTheory: String = THEORY
}
