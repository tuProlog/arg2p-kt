package it.unibo.tuprolog.argumentation.core.libraries.language.ruleNotation.notation

import it.unibo.tuprolog.argumentation.core.libraries.framework.loader.notation.ModuleNotation
import it.unibo.tuprolog.argumentation.engine.InternalArg2pApi
import it.unibo.tuprolog.core.operators.Operator
import it.unibo.tuprolog.core.operators.OperatorSet
import it.unibo.tuprolog.core.operators.Specifier

/**
 * The operators of the rules of arg2p (see RuleNotationLibrary), by part: each language of a profile is written with the
 * parts it reads (a strict rule, `r : a -> b`, is written with the operator of Prolog)
 */
@InternalArg2pApi
object RuleNotation {
    /** The name of a rule: `r : ...` */
    val label: OperatorSet = OperatorSet(Operator(":", Specifier.XFX, 1001))

    /** The defeasible rules: `r : a => b` */
    val defeasible: OperatorSet = OperatorSet(Operator("=>", Specifier.XFX, 1199))

    /** The ordinary premises: `f :=> a` */
    val premises: OperatorSet = OperatorSet(Operator(":=>", Specifier.XFX, 1199))

    /** The axioms: `f :-> a` */
    val axioms: OperatorSet = OperatorSet(Operator(":->", Specifier.XFX, 1199))

    /** The rules of a theory: `r : a => b`, `f :=> a`, `f :-> a` */
    val rules: OperatorSet = label + defeasible + premises + axioms

    /** The theories of the modules, with the rules */
    val theories: OperatorSet = ModuleNotation.theories.plus(rules)
}
