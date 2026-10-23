package it.unibo.tuprolog.argumentation.core.modules

import it.unibo.tuprolog.theory.Theory

/** A part of arg2p with a Prolog theory */
interface RawPrologContent {
    val prologTheory: Theory
}
