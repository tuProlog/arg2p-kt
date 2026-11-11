package it.unibo.tuprolog.argumentation.core.libraries.labelling.statement.statementPassThrough

import it.unibo.tuprolog.argumentation.core.libraries.PrologLibrary
import it.unibo.tuprolog.argumentation.core.libraries.labelling.statement.statementPassThrough.prolog.THEORY

/**
 * The labels of the statements passed through from those of their arguments (module `statementPassThrough`, a
 * statementLabellingMode): a statement takes the label of each of its arguments, so it may have several.
 */
object StatementPassThroughLibrary : PrologLibrary("statementPassThrough") {
    override val prologRawTheory: String = THEORY
}
