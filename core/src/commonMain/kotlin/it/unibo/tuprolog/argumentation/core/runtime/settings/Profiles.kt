package it.unibo.tuprolog.argumentation.core.runtime.settings

import it.unibo.tuprolog.argumentation.core.flags.ArgsFlag
import it.unibo.tuprolog.argumentation.core.flags.AutoTransposition
import it.unibo.tuprolog.argumentation.core.flags.GraphBuilder
import it.unibo.tuprolog.argumentation.core.flags.GraphExtension
import it.unibo.tuprolog.argumentation.core.flags.MetaConflicts
import it.unibo.tuprolog.argumentation.core.flags.MetaRules
import it.unibo.tuprolog.argumentation.core.flags.QueryMode
import it.unibo.tuprolog.core.Atom
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.Truth

/**
 * A framework a theory is read in, the value of graphBuildMode: a shortcut for the values of the flags it presets.
 * A flag the theory states keeps the value it states (a switch can only be turned on), the flags the profile does not
 * preset keep those of the solver.
 */
class Profile internal constructor(
    /** The value of graphBuildMode naming the profile */
    val name: String,
    /** What the profile reads a theory as */
    val description: String,
    presets: List<Pair<ArgsFlag<*, *>, List<Term>>>,
) {
    /** The values of the flags the profile presets, by flag (none for a switch off) */
    val presets: Map<String, List<Term>> = presets.associate { (flag, values) -> flag.predicate() to values }
}

/** The profiles of arg2p (see [Profile]) */
object Profiles {
    private fun on(flag: ArgsFlag<*, *>) = flag to listOf<Term>(Truth.TRUE)

    private fun off(flag: ArgsFlag<*, *>) = flag to emptyList<Term>()

    private fun values(
        flag: ArgsFlag<*, *>,
        vararg values: String,
    ) = flag to values.map { Atom.of(it) }

    /** No preset: every flag as the solver and the theory state it */
    val FREE = Profile("free", "No preset: every flag as the solver and the theory state it", emptyList())

    /**
     * ASPIC+ (Modgil and Prakken 2013, 2014): strict and defeasible rules, axioms and ordinary premises, contraries,
     * preferences weighed as orderingPrinciple and orderingComparator state; the strict rules closed under
     * transposition, which the rationality postulates need (Caminada and Amgoud 2007)
     */
    val ASPIC_PLUS =
        Profile(
            "aspic_plus",
            "ASPIC+: strict and defeasible rules, axioms and premises, preferences, strict rules closed under transposition",
            listOf(
                values(GraphBuilder, "ruleGraph"),
                values(GraphExtension, "standardPref"),
                on(AutoTransposition),
                off(MetaRules),
                off(MetaConflicts),
            ),
        )

    /** ASPIC+ with no preferences: every attack succeeds */
    val ASPIC =
        Profile(
            "aspic",
            "ASPIC+ with no preferences: every attack succeeds",
            listOf(values(GraphBuilder, "ruleGraph"), values(GraphExtension), on(AutoTransposition), off(MetaRules), off(MetaConflicts)),
        )

    /**
     * The defeasible part of ASPIC+: defeasible rules and ordinary premises, preferences; no strict rule nor axiom
     * (a theory with some is refused). Strict closure and the consistency postulates hold with nothing to close.
     */
    val ASPIC_DEFEASIBLE =
        Profile(
            "aspic_defeasible",
            "The defeasible part of ASPIC+: defeasible rules and premises, preferences, no strict rule nor axiom",
            listOf(
                values(GraphBuilder, "ruleGraph"),
                values(GraphExtension, "standardPref"),
                off(AutoTransposition),
                off(MetaRules),
                off(MetaConflicts),
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
     * Dung frameworks: the arguments (arg/1) and the attacks (att/2) the theory states, or derives with Prolog
     * clauses; an argument attacking or attacked is an argument as well
     */
    val ABSTRACT =
        Profile(
            "abstract",
            "Dung frameworks: the arguments (arg/1) and attacks (att/2) the theory states or derives",
            listOf(
                values(GraphBuilder, "abstractGraph"),
                values(GraphExtension),
                off(QueryMode),
                off(AutoTransposition),
                off(MetaRules),
                off(MetaConflicts),
            ),
        )

    /** Every profile */
    val all: List<Profile> = listOf(FREE, ASPIC_PLUS, ASPIC, ASPIC_DEFEASIBLE, META, ABSTRACT)

    /** The profile named [name], if any */
    fun named(name: String?): Profile? = all.firstOrNull { it.name == name }
}
