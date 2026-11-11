package it.unibo.tuprolog.argumentation.core.libraries.framework.settings.helpers

import it.unibo.tuprolog.argumentation.Profile
import it.unibo.tuprolog.argumentation.core.libraries.ArgsFlag
import it.unibo.tuprolog.argumentation.core.libraries.evaluation.pipeline.flags.GraphBuilder
import it.unibo.tuprolog.argumentation.core.libraries.evaluation.pipeline.flags.GraphExtension
import it.unibo.tuprolog.argumentation.core.libraries.evaluation.pipeline.flags.QueryMode
import it.unibo.tuprolog.argumentation.core.libraries.framework.parser.flags.GraphLanguage
import it.unibo.tuprolog.argumentation.core.libraries.framework.settings.flags.GraphBuildMode
import it.unibo.tuprolog.argumentation.core.libraries.graph.structured.extension.preference.superiority.flags.OrderingComparator
import it.unibo.tuprolog.argumentation.core.libraries.graph.structured.extension.preference.superiority.flags.OrderingPrinciple
import it.unibo.tuprolog.argumentation.core.libraries.language.conflict.flags.ConflictModel
import it.unibo.tuprolog.argumentation.core.libraries.language.conflict.flags.MetaConflicts
import it.unibo.tuprolog.argumentation.core.libraries.language.metaRules.flags.MetaRules
import it.unibo.tuprolog.argumentation.core.libraries.language.plainProlog.flags.PrologStrictCompatibility
import it.unibo.tuprolog.argumentation.core.libraries.language.transposition.flags.AutoTransposition
import it.unibo.tuprolog.core.Atom
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.Truth
import it.unibo.tuprolog.argumentation.ConflictModel as ConflictModels
import it.unibo.tuprolog.argumentation.GraphBuilder as GraphBuilders
import it.unibo.tuprolog.argumentation.GraphExtension as GraphExtensions
import it.unibo.tuprolog.argumentation.GraphLanguage as GraphLanguages
import it.unibo.tuprolog.argumentation.OrderingComparator as OrderingComparators
import it.unibo.tuprolog.argumentation.OrderingPrinciple as OrderingPrinciples

/**
 * The flags a profile presets ([profile], the value of graphBuildMode): a shortcut for their values. The flags the theory or
 * the solver state override it (see Settings), the flags the profile does not preset keep those of the solver.
 */
internal class Preset(
    val profile: Profile,
    presets: List<Pair<ArgsFlag, List<Term>>>,
) {
    /** The value of graphBuildMode naming the profile */
    val name: String get() = profile.value

    /** The values of the flags the profile presets, by flag (none for a switch off) */
    val presets: Map<String, List<Term>> = presets.associate { (flag, values) -> flag.predicate() to values }
}

/** What each profile of arg2p (see [Profile]) presets, ASPIC+ the default */
internal object Profiles {
    private fun on(flag: ArgsFlag) = flag to listOf<Term>(Truth.TRUE)

    private fun off(flag: ArgsFlag) = flag to emptyList<Term>()

    private fun values(
        flag: ArgsFlag,
        vararg values: String,
    ) = flag to values.map { Atom.of(it) }

    // The flags of the rule-based profiles beyond the extensions: no reasoning about rules or conflicts
    private fun rules(
        transposition: Boolean,
        vararg extensions: GraphExtensions,
    ) = listOf(
        values(GraphBuilder, GraphBuilders.RULE_GRAPH.value),
        values(GraphExtension, *extensions.map { it.value }.toTypedArray()),
        if (transposition) on(AutoTransposition) else off(AutoTransposition),
        off(MetaRules),
        off(MetaConflicts),
    )

    private fun language(language: GraphLanguages) = values(GraphLanguage, language.value)

    private fun presetOf(profile: Profile): Preset =
        Preset(
            profile,
            when (profile) {
                // no preset: every flag as the solver and the theory state it
                Profile.FREE -> emptyList()
                // ASPIC+ (Modgil and Prakken 2013, 2014): strict and defeasible rules, axioms and ordinary premises,
                // contraries, preferences weighed as orderingPrinciple and orderingComparator state, restricted rebut
                // (only on a conclusion of a defeasible rule); the strict rules closed under transposition, which the
                // rationality postulates need (Caminada and Amgoud 2007)
                Profile.ASPIC_PLUS ->
                    rules(true, GraphExtensions.STANDARD_PREF, GraphExtensions.REBUT_RESTRICTION) +
                        language(GraphLanguages.ASPIC_LANGUAGE)
                // ASPIC+ with no preferences: every attack succeeds
                Profile.ASPIC -> rules(true, GraphExtensions.REBUT_RESTRICTION) + language(GraphLanguages.ASPIC_LANGUAGE)
                // the defeasible part of ASPIC+: defeasible rules alone, facts as rules with no premise (`r : [] => a.`),
                // their preferences weighed by the last link, normal comparator; a theory with strict rules, axioms or
                // premises is refused. Strict closure and the consistency postulates hold with nothing to close.
                Profile.ASPIC_DEFEASIBLE ->
                    rules(false, GraphExtensions.STANDARD_PREF) +
                        listOf(
                            language(GraphLanguages.DEFEASIBLE_LANGUAGE),
                            values(OrderingPrinciple, OrderingPrinciples.LAST.value),
                            values(OrderingComparator, OrderingComparators.NORMAL.value),
                        )
                // ASPIC+ reasoning about its own rules, conflicts and preferences
                Profile.META ->
                    listOf(
                        values(GraphBuilder, GraphBuilders.RULE_GRAPH.value),
                        values(GraphExtension, GraphExtensions.DEFEASIBLE_PREF.value),
                        on(MetaRules),
                        on(MetaConflicts),
                    )
                // flat assumption-based argumentation (Bondarenko et al. 1997; Dung, Kowalski and Toni 2009), as the
                // instance of ASPIC+ it is (Prakken 2010): strict rules, assumptions (assumption/1) as ordinary premises,
                // contraries (contrary(Assumption, Contrary)) one way only, no preference, no other conflict (not even
                // negation). A theory with defeasible rules, or an assumption a rule concludes (not flat), is refused: the
                // semantics of non-flat ABA are those of sets of assumptions, which the arguments do not capture.
                Profile.ABA -> rules(false) + aba()
                // ABA+ (Čyras and Toni 2016): flat ABA with preferences between assumptions, `sup(Stronger, Weaker)`, an
                // attack by an argument resting on a weaker assumption reversed (see the extension abaPref). The theory
                // satisfies Weak Contraposition (Axiom 4 of Čyras and Toni), and is refused otherwise.
                Profile.ABA_PLUS -> rules(false, GraphExtensions.ABA_PREF) + aba()
                // Dung frameworks: the arguments, `a.` (or arg(a)), and the attacks, `a -> b.` (or att(a, b)), and
                // `a <-> b.` for a mutual one, stated or derived by Prolog clauses; a theory with the rules, premises,
                // preferences or conflicts of the structured profiles is refused
                Profile.ABSTRACT ->
                    listOf(
                        values(GraphBuilder, GraphBuilders.ABSTRACT_GRAPH.value),
                        language(GraphLanguages.ABSTRACT_LANGUAGE),
                        values(GraphExtension),
                        off(QueryMode),
                        off(AutoTransposition),
                        off(PrologStrictCompatibility),
                        off(MetaRules),
                        off(MetaConflicts),
                    )
            },
        )

    private fun aba() =
        listOf(language(GraphLanguages.ABA_LANGUAGE), values(ConflictModel, ConflictModels.DECLARED.value), off(PrologStrictCompatibility))

    /** Every profile, with what it presets */
    val all: List<Preset> = Profile.entries.map { presetOf(it) }

    /** The profile of a theory naming none */
    val default: Preset = all.first { it.name == GraphBuildMode.default() }

    /** The profile named [name], if any */
    fun named(name: String?): Preset? = all.firstOrNull { it.name == name }
}
