package it.unibo.tuprolog.argumentation.core.libraries.framework.parser.flags

import it.unibo.tuprolog.argumentation.core.libraries.ArgsFlag
import it.unibo.tuprolog.argumentation.GraphLanguage as GraphLanguageValue

/**
 * The languages extending the one of arg2p, each a module the parser applies (see parser::extension/1), for the premises and the conflicts its
 * notation states, mapped to those of arg2p, and that checks the theory is written in it: abaLanguage (assumptions
 * and contraries), defeasibleLanguage (defeasible rules alone), aspicLanguage (well-formed contraries), abstractLanguage (arguments
 * and attacks alone). The profile of the theory attaches them.
 */
object GraphLanguage : ArgsFlag {
    override fun predicate(): String = "graphLanguage"

    override fun default(): Iterable<String> = emptyList()

    override val isOpen: Boolean = true

    override fun values(): Iterable<String> = GraphLanguageValue.entries.map { it.value }
}
