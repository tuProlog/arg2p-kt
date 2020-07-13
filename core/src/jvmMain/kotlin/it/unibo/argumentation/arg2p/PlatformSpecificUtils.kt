package it.unibo.argumentation.arg2p

import it.unibo.tuprolog.theory.Theory
import it.unibo.tuprolog.theory.parsing.parse

actual fun loadTheoryFromFile(fileName: String): Theory {
    return Arg2pLibrary::class.java.classLoader.getResource(fileName).let {
        Theory.parse(it!!.readText())
    }
}