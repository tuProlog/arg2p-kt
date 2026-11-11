package it.unibo.tuprolog.argumentation.core.libraries.framework.parser

import it.unibo.tuprolog.argumentation.core.libraries.ArgsFlag
import it.unibo.tuprolog.argumentation.core.libraries.PrologLibrary
import it.unibo.tuprolog.argumentation.core.libraries.framework.parser.flags.GraphLanguage
import it.unibo.tuprolog.argumentation.core.libraries.framework.parser.primitives.TheoryClause
import it.unibo.tuprolog.argumentation.core.libraries.framework.parser.prolog.THEORY
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.primitive.Primitive

/**
 * The way the theories go into arg2p (module `parser`): readTheory/2 reads the rules and premises of a theory as the
 * extensions of its language give them (see extension/1: the notation of rules, the languages of the profile, the graph
 * extensions, the libraries reading a theory), which the store keeps and serves (see store::rulesRead/0); checkLanguages/0
 * checks the theory is written in its languages; hook/2 gives the base modules the extension extending them where they
 * call it; theoryClause/2 reads the clauses of the theory as data, for the languages. It knows no notation: the languages do.
 */
object ParserLibrary : PrologLibrary("parser") {
    override val primitives: Map<Signature, Primitive>
        get() = mapOf(TheoryClause.signature to TheoryClause)

    override val baseFlags: Iterable<ArgsFlag>
        get() = listOf(GraphLanguage)

    override val prologRawTheory: String = THEORY
}
