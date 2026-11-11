package it.unibo.tuprolog.argumentation.core.libraries.labelling.statement.statementBinary

import it.unibo.tuprolog.argumentation.core.libraries.PrologLibrary
import it.unibo.tuprolog.argumentation.core.libraries.labelling.statement.statementBinary.prolog.THEORY

/**
 * Two labels for the statements (module `statementBinary`, a statementLabellingMode): IN if an argument for the
 * statement is IN, OUT otherwise.
 */
object StatementBinaryLibrary : PrologLibrary("statementBinary") {
    override val prologRawTheory: String = THEORY
}
