package it.unibo.tuprolog.argumentation.core.libs.structured

import it.unibo.tuprolog.argumentation.core.libs.ArgLibrary
import it.unibo.tuprolog.argumentation.core.libs.ArgsFlag
import it.unibo.tuprolog.argumentation.core.libs.LazyRawPrologContent
import it.unibo.tuprolog.argumentation.core.libs.Loadable
import it.unibo.tuprolog.argumentation.core.libs.basic.Arg2pUnificator
import it.unibo.tuprolog.argumentation.core.libs.basic.DynamicLoader
import it.unibo.tuprolog.core.Atom
import it.unibo.tuprolog.core.Substitution
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.operators.OperatorSet
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.library.Library
import it.unibo.tuprolog.solve.primitive.BinaryRelation
import it.unibo.tuprolog.solve.primitive.Solve

abstract class StructuredModeBase :
    LazyRawPrologContent(),
    ArgLibrary,
    Loadable {
    override val alias = "prolog.argumentation.structured"

    override val baseContent: Library
        get() =
            Library.of(
                alias = this.alias,
                clauses = this.prologTheory,
                primitives = mapOf(GroundVars.descriptionPair),
            )
    override val baseFlags: Iterable<ArgsFlag<*, *>>
        get() = listOf(QueryMode, AmbiguityBlocking)

    override fun identifier(): String = "structured"

    override val theoryOperators =
        DynamicLoader
            .operators()
            .plus(OperatorSet.DEFAULT)
}

expect object StructuredMode : StructuredModeBase {
    override val prologRawTheory: String
}

object QueryMode : ArgsFlag<Boolean, Unit> {
    override fun predicate(): String = "queryMode"

    override val isSwitch: Boolean = true

    override fun default(): Boolean = true

    override fun values() {}
}

object AmbiguityBlocking : ArgsFlag<Boolean, Unit> {
    override fun predicate(): String = "ambiguityBlocking"

    override val isSwitch: Boolean = true

    override fun default(): Boolean = true

    override fun values() {}
}

// ground_vars(+Term, ?Grounded): Term with each of its variables replaced by the atom var
object GroundVars : BinaryRelation.WithoutSideEffects<ExecutionContext>("ground_vars") {
    private val placeholder = Atom.of("var")

    override fun Solve.Request<ExecutionContext>.computeAllSubstitutions(
        first: Term,
        second: Term,
    ): Sequence<Substitution> =
        Arg2pUnificator
            .mgu(second, first.apply(Substitution.of(first.variables.distinct().associateWith { placeholder })))
            .let { if (it.isSuccess) sequenceOf(it) else emptySequence() }
}
