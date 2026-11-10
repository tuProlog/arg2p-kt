package it.unibo.tuprolog.argumentation.core.libraries.language.plainProlog

import it.unibo.tuprolog.argumentation.core.libraries.ArgsFlag
import it.unibo.tuprolog.argumentation.core.libraries.PrologLibrary
import it.unibo.tuprolog.argumentation.core.libraries.language.plainProlog.flags.PrologStrictCompatibility
import it.unibo.tuprolog.argumentation.core.libraries.language.plainProlog.primitives.PrologAxioms
import it.unibo.tuprolog.argumentation.core.libraries.language.plainProlog.primitives.PrologDefeasibleRules
import it.unibo.tuprolog.argumentation.core.libraries.language.plainProlog.primitives.PrologPremises
import it.unibo.tuprolog.argumentation.core.libraries.language.plainProlog.primitives.PrologStrictRules
import it.unibo.tuprolog.argumentation.core.libraries.language.plainProlog.prolog.THEORY
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.primitive.Primitive

/**
 * Theories in plain Prolog (module `plainProlog`, with the flag prologStrictCompatibility): the clauses of a theory
 * read as its rules, premises and axioms, those named in a comment defeasible (see
 * [it.unibo.tuprolog.argumentation.core.libraries.framework.parser.helpers.RuleAnnotations]).
 */
object PlainPrologLibrary : PrologLibrary("plainProlog") {
    override val primitives: Map<Signature, Primitive>
        get() =
            mapOf(
                PrologStrictRules.signature to PrologStrictRules,
                PrologAxioms.signature to PrologAxioms,
                PrologPremises.signature to PrologPremises,
                PrologDefeasibleRules.signature to PrologDefeasibleRules,
            )

    override val baseFlags: Iterable<ArgsFlag>
        get() = listOf(PrologStrictCompatibility)

    override val extensionFlag: ArgsFlag
        get() = PrologStrictCompatibility

    override val prologRawTheory: String = THEORY
}
