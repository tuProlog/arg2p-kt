package it.unibo.argumentation.arg2p

import it.unibo.tuprolog.theory.Theory
import it.unibo.tuprolog.theory.parsing.parse

actual object Sources {

    private fun loadTheoryFromPrologFile(fileName: String): Theory {
        return Sources::class.java.getResource("$fileName.pl").let {
            Theory.parse(it!!.readText())
        }
    }

    actual val argumentationEngineInterface: Theory
        get() = loadTheoryFromPrologFile("argumentationEngineInterface")

    actual val argumentationGraph: Theory
        get() = loadTheoryFromPrologFile("argumentationGraph")

    actual val argumentBPLabelling: Theory
        get() = loadTheoryFromPrologFile("argumentBPLabelling")

    actual val argumentLabelling: Theory
        get() = loadTheoryFromPrologFile("argumentLabelling")

    actual val debug: Theory
        get() = loadTheoryFromPrologFile("debug")

    actual val ruleTranslator: Theory
        get() = loadTheoryFromPrologFile("ruleTranslator")

    actual val statementLabelling: Theory
        get() = loadTheoryFromPrologFile("statementLabelling")

    actual val utils: Theory
        get() = loadTheoryFromPrologFile("utils")
}