package it.unibo.tuprolog.argumentation.core

import it.unibo.tuprolog.argumentation.core.modularity.ModuleCall
import it.unibo.tuprolog.core.operators.Operator
import it.unibo.tuprolog.core.operators.OperatorSet
import it.unibo.tuprolog.core.operators.Specifier
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
        operatorSet = OperatorSet(
            Operator("=>", Specifier.XFX, 1199),
            Operator(":=>", Specifier.XFX, 1199),
            Operator(":->", Specifier.XFX, 1199),
            Operator(":", Specifier.XFX, 1001)
        ),
        theory = theories.reduce(Theory::plus),
        primitives = mapOf(ModuleCall.signature to ModuleCall::invoke),
        alias = "prolog.argumentation"
    )
