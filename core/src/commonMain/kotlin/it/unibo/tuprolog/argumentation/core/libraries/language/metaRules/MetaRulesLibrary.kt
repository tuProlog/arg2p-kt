package it.unibo.tuprolog.argumentation.core.libraries.language.metaRules

import it.unibo.tuprolog.argumentation.core.libraries.ArgsFlag
import it.unibo.tuprolog.argumentation.core.libraries.PrologLibrary
import it.unibo.tuprolog.argumentation.core.libraries.language.metaRules.flags.MetaRules
import it.unibo.tuprolog.argumentation.core.libraries.language.metaRules.prolog.THEORY

/**
 * The rules as statements of the theory (module `metaRules`, with the flag of the same name): each rule `r` has the
 * premise `applicable(r)` before those written, so that the theory argues about which of its rules apply (as
 * conflictArguments makes it argue about its conflicts). The rules are read so, once (see parser::readTheory/2 and its
 * hook rewrite/4): the store keeps them with that premise, and a theory partitioned among solvers is written with it.
 */
object MetaRulesLibrary : PrologLibrary("metaRules") {
    override val baseFlags: Iterable<ArgsFlag>
        get() = listOf(MetaRules)

    override val extensionFlag: ArgsFlag
        get() = MetaRules

    override val prologRawTheory: String = THEORY
}
