package it.unibo.argumentation

import it.unibo.tuprolog.theory.Theory

expect object Sources {
    val argumentationEngineInterface: Theory
    val abstractMode: Theory
    val queryMode: Theory
    val argumentationGraph: Theory
    val prefArgumentationGraph: Theory
    val bpLabelling: Theory
    val completeLabelling: Theory
    val groundedLabelling: Theory
    val statementLabelling: Theory
    val ruleTranslator: Theory
    val debug: Theory
    val utils: Theory
}
