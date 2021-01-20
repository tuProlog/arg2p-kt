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

    actual val abstractMode: Theory
        get() = loadTheoryFromPrologFile("core/abstractMode")

    actual val queryMode: Theory
        get() = loadTheoryFromPrologFile("core/queryMode")

    actual val argumentationGraph: Theory
        get() = loadTheoryFromPrologFile("core/graph/argumentationGraph")

    actual val prefArgumentationGraph: Theory
        get() = loadTheoryFromPrologFile("core/graph/prefArgumentationGraph")

    actual val bpLabelling: Theory
        get() = loadTheoryFromPrologFile("core/labellings/argument/bpPartialComplete")

    actual val groundedLabelling: Theory
        get() = loadTheoryFromPrologFile("core/labellings/argument/grounded")

    actual val completeLabelling: Theory
        get() = loadTheoryFromPrologFile("core/labellings/argument/complete")

    actual val statementLabelling: Theory
        get() = loadTheoryFromPrologFile("core/labellings/statement/statementLabelling")

    actual val ruleTranslator: Theory
        get() = loadTheoryFromPrologFile("core/translation/ruleTranslator")

    actual val debug: Theory
        get() = loadTheoryFromPrologFile("utils/debug")

    actual val utils: Theory
        get() = loadTheoryFromPrologFile("utils/utils")
}
