package it.unibo.tuprolog.argumentation.core.modules

import it.unibo.tuprolog.theory.Theory
import it.unibo.tuprolog.theory.parsing.parse

/** A part of arg2p whose Prolog theory is a text ([prologRawTheory]), parsed at the first use */
abstract class LazyRawPrologContent :
    ArgLibrary,
    RawPrologContent {
    abstract val prologRawTheory: String
    override val prologTheory: Theory by lazy {
        Theory.parse(prologRawTheory, theoryOperators)
    }
}
