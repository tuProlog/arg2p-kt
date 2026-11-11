package it.unibo.tuprolog.argumentation.core.libraries.labelling.statement.statement

import it.unibo.tuprolog.argumentation.core.libraries.PrologLibrary
import it.unibo.tuprolog.argumentation.core.libraries.labelling.statement.statement.prolog.THEORY

/**
 * The labels of the statements from those of their arguments (module `statement`, a statementLabellingMode): a
 * statement is IN if an argument for it is IN, otherwise OUT if one is OUT, otherwise UND if one is UND.
 */
object StatementLibrary : PrologLibrary("statement") {
    override val prologRawTheory: String = THEORY
}
