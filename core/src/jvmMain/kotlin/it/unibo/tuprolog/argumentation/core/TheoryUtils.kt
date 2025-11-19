package it.unibo.tuprolog.argumentation.core

object TheoryUtils {
    fun loadTheoryFromPrologFile(fileName: String): String =
        TheoryUtils::class.java.getResource("$fileName.pl").let {
            it!!.readText()
        }
}
