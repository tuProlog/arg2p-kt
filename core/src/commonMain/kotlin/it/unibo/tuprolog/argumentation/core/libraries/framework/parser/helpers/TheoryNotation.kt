package it.unibo.tuprolog.argumentation.core.libraries.framework.parser.helpers

import it.unibo.tuprolog.argumentation.core.libraries.Loadable
import it.unibo.tuprolog.argumentation.core.libraries.framework.loader.notation.ModuleNotation
import it.unibo.tuprolog.argumentation.core.libraries.framework.parser.flags.GraphLanguage
import it.unibo.tuprolog.argumentation.core.libraries.framework.settings.flags.GraphBuildMode
import it.unibo.tuprolog.argumentation.core.libraries.framework.settings.helpers.Profiles
import it.unibo.tuprolog.argumentation.engine.InternalArg2pApi
import it.unibo.tuprolog.argumentation.engine.term.Arg2pUnificator
import it.unibo.tuprolog.core.Atom
import it.unibo.tuprolog.core.Clause
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.operators.OperatorSet
import it.unibo.tuprolog.theory.Theory
import it.unibo.tuprolog.unify.Unificator

/**
 * The notation a theory is read with: that of the languages it is written in, and nothing else. A theory names its
 * languages through its flags (graphLanguage, or the profile, graphBuildMode, presetting it), which are facts read with
 * the notation of Prolog alone (see [TheoryText.facts]); as the settings do (see Settings), the theory states them, or else
 * the settings of the solver, or else the profile, the notation of the rules of arg2p with no language. A theory using the
 * notation of a language it is not written in does not read: `f :=> a.` in an abstract framework is a syntax error.
 */
@InternalArg2pApi
object TheoryNotation {
    private val language = GraphLanguage.predicate()
    private val profile = GraphBuildMode.predicate()

    // the language of a theory naming none
    private const val RULES = "ruleNotation"

    /**
     * The languages of [text], with the facts of [settings] where it states none, each source of them before those after
     * it (the theory already read, then the settings of the solver)
     */
    fun languagesOf(
        text: String,
        settings: List<Iterable<Clause>>,
    ): Set<String> {
        val sources =
            listOf(TheoryText.facts(text, setOf(language, profile))) +
                settings.map { clauses -> clauses.filter { it.isFact }.mapNotNull { it.head } }

        fun stated(name: String): List<Term> =
            sources
                .asSequence()
                .map { valuesOf(name, it) }
                .firstOrNull { it.isNotEmpty() }
                .orEmpty()
        val named =
            stated(language).ifEmpty {
                val mode = stated(profile).firstOrNull()
                val preset = mode?.let { Profiles.named((it as? Atom)?.value) } ?: Profiles.default
                preset.presets[language].orEmpty()
            }
        return named
            .mapNotNull { (it as? Atom)?.value }
            .filter { it != "none" }
            .toSet()
            .ifEmpty { setOf(RULES) }
    }

    /** The operators of [text]: those of the modules, and the notations of its languages among [libraries] */
    fun of(
        text: String,
        settings: List<Iterable<Clause>>,
        libraries: Iterable<Loadable>,
    ): OperatorSet {
        val languages = languagesOf(text, settings)
        return libraries.filter { it.identifier() in languages }.fold(ModuleNotation.theories) { all, library -> all + library.notation }
    }

    /** The clauses of [text], read with its notation (see [of]), their comments as [RuleAnnotations] reads them */
    fun read(
        text: String,
        settings: List<Iterable<Clause>>,
        libraries: Iterable<Loadable>,
        unificator: Unificator = Arg2pUnificator,
    ): Theory = RuleAnnotations.parse(text, of(text, settings, libraries), unificator)

    private fun valuesOf(
        name: String,
        facts: List<Struct>,
    ): List<Term> = facts.filter { it.functor == name && it.arity == 1 }.map { it[0] }
}
