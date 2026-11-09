package it.unibo.tuprolog.argumentation.core.libraries.language.naturalTerms

import it.unibo.tuprolog.argumentation.core.libraries.ArgsFlag
import it.unibo.tuprolog.argumentation.core.libraries.PrologLibrary
import it.unibo.tuprolog.argumentation.core.libraries.language.naturalTerms.flags.NaturalTerms
import it.unibo.tuprolog.argumentation.core.libraries.language.naturalTerms.primitives.RuleToClauseNatural
import it.unibo.tuprolog.argumentation.core.libraries.language.naturalTerms.prolog.THEORY
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.primitive.Primitive

/**
 * Theories written in natural language (module `naturalTerms`, with the flag of that name): the statements written as
 * text are turned into terms, when the rules read are kept as clauses (see parser::clausesOf/4).
 */
object NaturalTermsLibrary : PrologLibrary("naturalTerms") {
    override val primitives: Map<Signature, Primitive>
        get() = mapOf(RuleToClauseNatural.signature to RuleToClauseNatural)

    override val baseFlags: Iterable<ArgsFlag>
        get() = listOf(NaturalTerms)

    override val parsingFlag: ArgsFlag
        get() = NaturalTerms

    override val prologRawTheory: String = THEORY
}
