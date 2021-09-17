package it.unibo.tuprolog.argumentation.core

import it.unibo.tuprolog.argumentation.core.modularity.ModuleCall
import it.unibo.tuprolog.argumentation.core.primitives.Axioms1
import it.unibo.tuprolog.argumentation.core.primitives.Bps1
import it.unibo.tuprolog.argumentation.core.primitives.DefeasibleRules1
import it.unibo.tuprolog.argumentation.core.primitives.Premises1
import it.unibo.tuprolog.argumentation.core.primitives.StrictRules1
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
    Sources.preferences,
    Sources.defPreferences,
    Sources.genericDefPreferences,
    Sources.attackRestriction,
    Sources.bpArgumentationGraph,
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
    alias = "prolog.argumentation",
    primitives = mapOf(
        ModuleCall.signature to ModuleCall,
        StrictRules1::descriptionPair.get(),
        Axioms1::descriptionPair.get(),
        Bps1::descriptionPair.get(),
        Premises1::descriptionPair.get(),
        DefeasibleRules1::descriptionPair.get()
    ),
    theory = theories.reduce(Theory::plus),
    operatorSet = OperatorSet(
        Operator("=>", Specifier.XFX, 1199),
        Operator(":=>", Specifier.XFX, 1199),
        Operator(":->", Specifier.XFX, 1199),
        Operator(":", Specifier.XFX, 1001),
        Operator(":=", Specifier.XFX, 1199)
    )
)
