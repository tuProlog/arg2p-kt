// Forked from 2p-kt's solve-prolog 3.0.2 (Apache License 2.0): see README.md in this package
package it.unibo.tuprolog.argumentation.core.engine.stdlib

import it.unibo.tuprolog.argumentation.core.engine.stdlib.primitive.CopyTerm
import it.unibo.tuprolog.argumentation.core.engine.stdlib.primitive.FindAll
import it.unibo.tuprolog.argumentation.core.engine.stdlib.primitive.Length
import it.unibo.tuprolog.argumentation.core.engine.stdlib.primitive.Throw
import it.unibo.tuprolog.argumentation.core.engine.stdlib.rule.Call
import it.unibo.tuprolog.argumentation.core.engine.stdlib.rule.Catch
import it.unibo.tuprolog.argumentation.core.engine.stdlib.rule.Comma
import it.unibo.tuprolog.argumentation.core.engine.stdlib.rule.Cut
import it.unibo.tuprolog.argumentation.core.engine.stdlib.rule.NegationAsFailure
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.library.impl.ExtensionLibrary
import it.unibo.tuprolog.solve.primitive.Primitive
import it.unibo.tuprolog.solve.primitive.PrimitiveWrapper
import it.unibo.tuprolog.solve.rule.RuleWrapper
import it.unibo.tuprolog.solve.stdlib.CommonBuiltins

/**
 * The [it.unibo.tuprolog.solve.library.Library] of built-in predicates the prolog engine adds on top of
 * [CommonBuiltins]: this is what `it.unibo.tuprolog.argumentation.core.engine.Arg2pPrologSolverFactory.defaultBuiltins` returns,
 * and what a solver built via `Solver.prolog.solverWithDefaultBuiltins()` is loaded with.
 *
 * Everything here exists specifically because this engine resolves goals through the finite-state machine
 * under `it.unibo.tuprolog.argumentation.core.engine.fsm` rather than host-language recursion, so control constructs need an
 * engine-specific implementation: [it.unibo.tuprolog.argumentation.core.engine.stdlib.primitive.Throw] is a `Primitive`
 * (`throw/1`), while [it.unibo.tuprolog.argumentation.core.engine.stdlib.rule.Catch] (`catch/3`),
 * [it.unibo.tuprolog.argumentation.core.engine.stdlib.rule.Call] (`call/1`),
 * [it.unibo.tuprolog.argumentation.core.engine.stdlib.rule.Comma] (`','/2`),
 * [it.unibo.tuprolog.argumentation.core.engine.stdlib.rule.Cut] (`'!'/0`) and
 * [it.unibo.tuprolog.argumentation.core.engine.stdlib.rule.NegationAsFailure] (`'\+'/1`) are all `RuleWrapper`s expanded into
 * ordinary rule bodies that `StateRuleSelection`/`StateException` know how to special-case (cut transparency,
 * exception propagation) at the FSM level.
 */
object DefaultBuiltins : ExtensionLibrary(CommonBuiltins) {
    // copy_term/2 and findall/3 of the engine in place of those of 2p-kt, which copy terms by the names of their
    // variables (see CopyTerm): the library refuses to add a predicate it has, so they replace them here
    override val primitives: Map<Signature, Primitive> by lazy {
        super.primitives + listOf(CopyTerm, FindAll).map { it.descriptionPair }
    }

    override val additionalPrimitives: Iterable<PrimitiveWrapper<*>>
        get() =
            listOf(
                Throw,
                Length,
            )

    override val additionalRules: Iterable<RuleWrapper<*>>
        get() =
            listOf(
                Catch,
                Call,
                Comma,
                Cut,
                NegationAsFailure.Fail,
                NegationAsFailure.Success,
            )
}
