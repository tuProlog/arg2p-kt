package it.unibo.tuprolog.argumentation.core.runtime.settings

import it.unibo.tuprolog.argumentation.core.flags.ArgsFlag
import it.unibo.tuprolog.argumentation.core.flags.AutoTransposition
import it.unibo.tuprolog.argumentation.core.flags.ConflictModel
import it.unibo.tuprolog.argumentation.core.flags.GraphBuilder
import it.unibo.tuprolog.argumentation.core.flags.GraphExtension
import it.unibo.tuprolog.argumentation.core.flags.GraphLanguage
import it.unibo.tuprolog.argumentation.core.flags.MetaConflicts
import it.unibo.tuprolog.argumentation.core.flags.MetaRules
import it.unibo.tuprolog.argumentation.core.flags.OrderingComparator
import it.unibo.tuprolog.argumentation.core.flags.OrderingPrinciple
import it.unibo.tuprolog.argumentation.core.flags.PrologStrictCompatibility
import it.unibo.tuprolog.argumentation.core.flags.QueryMode
import it.unibo.tuprolog.core.Atom
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.Truth
import it.unibo.tuprolog.core.operators.Operator
import it.unibo.tuprolog.core.operators.OperatorSet
import it.unibo.tuprolog.core.operators.Specifier

/**
 * A framework a theory is read in, the value of graphBuildMode: a shortcut for the values of the flags it presets,
 * with the [operators] of its notation. The flags the theory or the solver state override it (see Settings),
 * the flags the profile does not preset keep those of the solver.
 */
internal class Profile(
    /** The value of graphBuildMode naming the profile */
    val name: String,
    /** What the profile reads a theory as */
    val description: String,
    presets: List<Pair<ArgsFlag<*, *>, List<Term>>>,
    /** The operators of the notation of the profile, beyond those of arg2p */
    val operators: OperatorSet = OperatorSet.EMPTY,
) {
    /** The values of the flags the profile presets, by flag (none for a switch off) */
    val presets: Map<String, List<Term>> = presets.associate { (flag, values) -> flag.predicate() to values }
}

/**
 * The profiles of arg2p (see [Profile]), ASPIC+ the default. The operators of all the profiles are those of every
 * theory, none conflicting with another: each profile reads the clauses written with them its own way.
 */
internal object Profiles {
    private fun on(flag: ArgsFlag<*, *>) = flag to listOf<Term>(Truth.TRUE)

    private fun off(flag: ArgsFlag<*, *>) = flag to emptyList<Term>()

    private fun values(
        flag: ArgsFlag<*, *>,
        vararg values: String,
    ) = flag to values.map { Atom.of(it) }

    // The flags of the rule-based profiles beyond the extensions: no reasoning about rules or conflicts
    private fun rules(
        transposition: Boolean,
        vararg extensions: String,
    ) = listOf(
        values(GraphBuilder, "ruleGraph"),
        values(GraphExtension, *extensions),
        if (transposition) on(AutoTransposition) else off(AutoTransposition),
        off(MetaRules),
        off(MetaConflicts),
    )

    /** No preset: every flag as the solver and the theory state it */
    val FREE = Profile("free", "No preset: every flag as the solver and the theory state it", emptyList())

    /**
     * ASPIC+ (Modgil and Prakken 2013, 2014): strict and defeasible rules, axioms and ordinary premises, contraries,
     * preferences weighed as orderingPrinciple and orderingComparator state, restricted rebut (only on a conclusion
     * of a defeasible rule, rebutRestriction); the strict rules closed under transposition, which the rationality
     * postulates need (Caminada and Amgoud 2007)
     */
    val ASPIC_PLUS =
        Profile(
            "aspic_plus",
            "ASPIC+: strict and defeasible rules, axioms and premises, preferences, restricted rebut, transposition",
            rules(true, "standardPref", "rebutRestriction") + values(GraphLanguage, "aspicLanguage"),
        )

    /** ASPIC+ with no preferences: every attack succeeds */
    val ASPIC =
        Profile(
            "aspic",
            "ASPIC+ with no preferences: every attack succeeds",
            rules(true, "rebutRestriction") + values(GraphLanguage, "aspicLanguage"),
        )

    /**
     * The defeasible part of ASPIC+: defeasible rules alone, facts as rules with no premise (`r : [] => a.`), their
     * preferences weighed by the last link, normal comparator; a theory with strict rules, axioms or premises is
     * refused. Strict closure and the consistency postulates hold with nothing to close.
     */
    val ASPIC_DEFEASIBLE =
        Profile(
            "aspic_defeasible",
            "The defeasible part of ASPIC+: defeasible rules alone, preferences by the last link (normal)",
            rules(false, "standardPref") +
                listOf(
                    values(GraphLanguage, "defeasibleLanguage"),
                    values(OrderingPrinciple, "last"),
                    values(OrderingComparator, "normal"),
                ),
        )

    /** ASPIC+ reasoning about its own rules, conflicts and preferences: metaRules, metaConflicts, defeasiblePref */
    val META =
        Profile(
            "meta",
            "ASPIC+ reasoning about its rules, conflicts and preferences (metaRules, metaConflicts, defeasiblePref)",
            listOf(values(GraphBuilder, "ruleGraph"), values(GraphExtension, "defeasiblePref"), on(MetaRules), on(MetaConflicts)),
        )

    /**
     * Flat assumption-based argumentation (Bondarenko et al. 1997; Dung, Kowalski and Toni 2009), as the instance of
     * ASPIC+ it is (Prakken 2010): strict rules, assumptions (assumption/1) as ordinary premises, contraries
     * (contrary(Assumption, Contrary)) one way only, no preference, no other conflict (not even negation). A theory
     * with defeasible rules, or an assumption a rule concludes (not flat), is refused: the semantics of non-flat ABA
     * are those of sets of assumptions, which the arguments do not capture.
     */
    val ABA =
        Profile(
            "aba",
            "Flat ABA: strict rules, assumptions (assumption/1), their contraries (contrary/2), no preference",
            rules(false) + listOf(values(GraphLanguage, "abaLanguage"), values(ConflictModel, "declared"), off(PrologStrictCompatibility)),
        )

    /**
     * ABA+ (Čyras and Toni 2016): flat ABA with preferences between assumptions, `sup(Stronger, Weaker)`, an attack
     * by an argument resting on a weaker assumption reversed (see the extension abaPref). The theory satisfies Weak
     * Contraposition (Axiom 4 of Čyras and Toni), and is refused otherwise: without it, the semantics of ABA+ are
     * those of sets of assumptions, which the arguments do not capture.
     */
    val ABA_PLUS =
        Profile(
            "aba_plus",
            "ABA+: flat ABA with preferences between assumptions (sup/2), attacks from weaker ones reversed",
            rules(false, "abaPref") +
                listOf(values(GraphLanguage, "abaLanguage"), values(ConflictModel, "declared"), off(PrologStrictCompatibility)),
        )

    /**
     * Dung frameworks: the arguments, `a.` (or arg(a)), and the attacks, `a -> b.` (or att(a, b)), and `a <-> b.` for
     * a mutual one, stated or derived by Prolog clauses (`X -> Y :- beats(X, Y).`); an argument attacking or attacked
     * is an argument as well. A theory with the rules, premises, preferences or conflicts of the structured profiles is
     * refused.
     */
    val ABSTRACT =
        Profile(
            "abstract",
            "Dung frameworks: arguments (a.), attacks (a -> b.) and mutual attacks (a <-> b.), stated or derived",
            listOf(
                values(GraphBuilder, "abstractGraph"),
                values(GraphLanguage, "abstractLanguage"),
                values(GraphExtension),
                off(QueryMode),
                off(AutoTransposition),
                off(PrologStrictCompatibility),
                off(MetaRules),
                off(MetaConflicts),
            ),
            OperatorSet(Operator("<->", Specifier.XFX, 1050)),
        )

    /** Every profile */
    val all: List<Profile> = listOf(FREE, ASPIC_PLUS, ASPIC, ASPIC_DEFEASIBLE, META, ABA, ABA_PLUS, ABSTRACT)

    /** The profile of a theory naming none */
    val default: Profile = ASPIC_PLUS

    /** The profile named [name], if any */
    fun named(name: String?): Profile? = all.firstOrNull { it.name == name }

    /** The operators of the notations of all the profiles */
    fun operators(): OperatorSet = all.fold(OperatorSet.EMPTY) { operators, profile -> operators + profile.operators }
}
