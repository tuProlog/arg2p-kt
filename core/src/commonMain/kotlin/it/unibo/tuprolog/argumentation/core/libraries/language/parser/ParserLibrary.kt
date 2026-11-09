package it.unibo.tuprolog.argumentation.core.libraries.language.parser

import it.unibo.tuprolog.argumentation.core.libraries.ArgsFlag
import it.unibo.tuprolog.argumentation.core.libraries.PrologLibrary
import it.unibo.tuprolog.argumentation.core.libraries.language.parser.flags.GraphLanguage
import it.unibo.tuprolog.argumentation.core.libraries.language.parser.flags.MetaRules
import it.unibo.tuprolog.argumentation.core.libraries.language.parser.language.RuleNotation
import it.unibo.tuprolog.argumentation.core.libraries.language.parser.language.primitives.RuleToClause
import it.unibo.tuprolog.argumentation.core.libraries.language.parser.prolog.THEORY
import it.unibo.tuprolog.core.operators.OperatorSet
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.primitive.Primitive

/**
 * A theory, read (module `parser`): rule/3, premise/2 and the indexes built once in the context give its rules and premises,
 * as the extensions of the language active give them (see extension/1), read the first time they are asked for; the
 * other modules need not know the language. checkLanguages/0 checks the theory is written in its languages. What is
 * read is kept in the cache as clauses: `rl(Conclusion) :- [Id, Premises, Conclusion]` for a rule, Premises a conjunction,
 * `rl(Conclusion) :- [Id, Conclusion]` for a premise (ruleClause/5, premiseClause/4; Kotlin builds them through
 * [it.unibo.tuprolog.argumentation.core.libraries.language.parser.helpers.RuleTerm]); premiseKind/2 tells the premises of a
 * rule apart.
 */
object ParserLibrary : PrologLibrary("parser") {
    override val primitives: Map<Signature, Primitive>
        get() = mapOf(RuleToClause.signature to RuleToClause)

    // The operators of the rules of a theory, for the theory of the parser and the solvers using it
    override val libraryOperators: OperatorSet
        get() = RuleNotation.rules

    override val theoryOperators: OperatorSet
        get() = RuleNotation.theories

    override val notation: OperatorSet
        get() = RuleNotation.rules

    override val baseFlags: Iterable<ArgsFlag>
        get() = listOf(GraphLanguage, MetaRules)

    override val prologRawTheory: String = THEORY
}
