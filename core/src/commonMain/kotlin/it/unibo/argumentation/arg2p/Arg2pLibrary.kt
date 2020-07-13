package it.unibo.argumentation.arg2p

import it.unibo.tuprolog.core.operators.OperatorSet
import it.unibo.tuprolog.solve.library.AliasedLibrary
import it.unibo.tuprolog.solve.library.Libraries
import it.unibo.tuprolog.solve.library.Library
import it.unibo.tuprolog.theory.Theory

object Arg2pLibrary {

    private const val PATH = "it/unibo/argumentation/arg2p/"

    private val theories = sequenceOf(
        "utils.pl",
        "debug.pl",
        "ruleTranslator.pl",
        "argumentationGraph.pl",
        "argumentLabelling.pl",
        "argumentBPLabelling.pl",
        "statementLabelling.pl",
        "argumentationEngineInterface.pl"
    )

    val get : Libraries by lazy {
        Libraries(theories.map { getLibraryFromFile(it) })
    }

    private fun getLibraryFromFile(fileName: String) : AliasedLibrary =
        Library.aliased(
            alias = fileName,
            operatorSet = OperatorSet.DEFAULT,
            theory = loadTheoryFromFile(PATH + fileName)
        )
}