package it.unibo.argumentation.arg2p

import it.unibo.tuprolog.core.operators.OperatorSet
import it.unibo.tuprolog.solve.library.AliasedLibrary
import it.unibo.tuprolog.solve.library.Library
import it.unibo.tuprolog.theory.Theory

private val theories = sequenceOf(
    Sources.utils,
    Sources.debug,
    Sources.ruleTranslator,
    Sources.argumentationGraph,
    Sources.prefArgumentationGraph,
    Sources.groundedLabelling,
    Sources.completeLabelling,
    Sources.bpLabelling,
    Sources.statementLabelling,
    Sources.abstractMode,
    Sources.queryMode,
    Sources.argumentationEngineInterface
)

object Arg2p : AliasedLibrary by
    Library.aliased(
        operatorSet = OperatorSet.DEFAULT,
        theory = theories.reduce(Theory::plus),
        alias = "prolog.argumentation"
    )
