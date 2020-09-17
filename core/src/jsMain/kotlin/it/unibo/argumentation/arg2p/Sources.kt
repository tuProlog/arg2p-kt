package it.unibo.argumentation.arg2p

import it.unibo.tuprolog.theory.Theory
import it.unibo.tuprolog.theory.parsing.parse

actual object Sources {
    actual val argumentationEngineInterface: Theory
        get() = Theory.parse(ArgumentationEngineInterface.theoryCode)

    actual val argumentationGraph: Theory
        get() = Theory.parse(ArgumentationGraph.theoryCode)

    actual val argumentBPLabelling: Theory
        get() = Theory.parse(ArgumentBPLabelling.theoryCode)

    actual val argumentLabelling: Theory
        get() = Theory.parse(ArgumentLabelling.theoryCode)

    actual val debug: Theory
        get() = Theory.parse(Debug.theoryCode)

    actual val ruleTranslator: Theory
        get() = Theory.parse(RuleTranslator.theoryCode)

    actual val statementLabelling: Theory
        get() = Theory.parse(StatementLabelling.theoryCode)

    actual val utils: Theory
        get() = Theory.parse(Utils.theoryCode)
}