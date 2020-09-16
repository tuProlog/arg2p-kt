package it.unibo.argumentation.arg2p

import it.unibo.tuprolog.theory.Theory

expect object Sources {
    val argumentationEngineInterface: Theory
    val argumentationGraph: Theory
    val argumentBPLabelling: Theory
    val argumentLabelling: Theory
    val debug: Theory
    val ruleTranslator: Theory
    val statementLabelling: Theory
    val utils: Theory
}