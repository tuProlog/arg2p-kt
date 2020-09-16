package it.unibo.argumentation.arg2p

import it.unibo.tuprolog.theory.Theory
import it.unibo.tuprolog.theory.parsing.parse

actual object Sources {
    actual val argumentationEngineInterface: Theory
        get() = Theory.parse(
            it.unibo.argumentation.arg2p.argumentationEngineInterface
        )
    actual val argumentationGraph: Theory
        get() = Theory.parse(
            it.unibo.argumentation.arg2p.argumentationGraph
        )
    actual val argumentBPLabelling: Theory
        get() = Theory.parse(
            it.unibo.argumentation.arg2p.argumentBPLabelling
        )
    actual val argumentLabelling: Theory
        get() = Theory.parse(
            it.unibo.argumentation.arg2p.argumentLabelling
        )
    actual val debug: Theory
        get() = Theory.parse(
            it.unibo.argumentation.arg2p.debug
        )
    actual val ruleTranslator: Theory
        get() = Theory.parse(
            it.unibo.argumentation.arg2p.ruleTranslator
        )
    actual val statementLabelling: Theory
        get() = Theory.parse(
            it.unibo.argumentation.arg2p.statementLabelling
        )
    actual val utils: Theory
        get() = Theory.parse(
            it.unibo.argumentation.arg2p.utils
        )
}