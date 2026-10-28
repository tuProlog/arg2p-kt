package it.unibo.tuprolog.argumentation.core.flags

/**
 * The languages extending the one of arg2p, each a module the parser asks for the premises and the conflicts its
 * notation states, mapped to those of arg2p, and that checks the theory is written in it: abaLanguage (assumptions
 * and contraries), defeasibleLanguage (defeasible rules alone), aspicLanguage (well-formed contraries). The profile
 * of the theory attaches them.
 */
object GraphLanguage : ArgsFlag<Iterable<String>, Iterable<String>> {
    override fun predicate(): String = "graphLanguage"

    override fun default(): Iterable<String> = emptyList()

    override fun values(): Iterable<String> = listOf("aspicLanguage", "abaLanguage", "defeasibleLanguage")
}
