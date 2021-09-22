package it.unibo.tuprolog.argumentation.core

import it.unibo.tuprolog.theory.Theory
import it.unibo.tuprolog.theory.parsing.parse

object TheoryUtils {
    fun loadTheoryFromPrologFile(fileName: String): Theory {
        return TheoryUtils::class.java.getResource("$fileName.pl").let {
            Theory.parse(it!!.readText())
        }
    }
}