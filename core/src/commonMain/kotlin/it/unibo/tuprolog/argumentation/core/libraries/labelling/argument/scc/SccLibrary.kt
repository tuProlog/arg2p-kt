package it.unibo.tuprolog.argumentation.core.libraries.labelling.argument.scc

import it.unibo.tuprolog.argumentation.core.libraries.PrologLibrary
import it.unibo.tuprolog.argumentation.core.libraries.labelling.argument.scc.primitives.Forcing
import it.unibo.tuprolog.argumentation.core.libraries.labelling.argument.scc.primitives.LabelIn
import it.unibo.tuprolog.argumentation.core.libraries.labelling.argument.scc.primitives.Selection
import it.unibo.tuprolog.argumentation.core.libraries.labelling.argument.scc.primitives.UndecidedCandidates
import it.unibo.tuprolog.argumentation.core.libraries.labelling.argument.scc.primitives.UndecidedSccs
import it.unibo.tuprolog.argumentation.core.libraries.labelling.argument.scc.prolog.THEORY
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.primitive.Primitive

/**
 * Support module of the SCC-recursive semantics (admissible, complete, preferred, stable): their labellings are built one
 * strongly connected component at a time, the semantics only choosing which local labellings to keep.
 */
object SccLibrary : PrologLibrary("scc") {
    override val primitives: Map<Signature, Primitive>
        get() = listOf(UndecidedCandidates, UndecidedSccs, Selection, Forcing, LabelIn).associate { it.signature to it }

    override val prologRawTheory: String = THEORY
}
