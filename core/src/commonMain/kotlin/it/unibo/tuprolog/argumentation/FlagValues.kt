package it.unibo.tuprolog.argumentation

import kotlin.js.JsExport

// The values the flags of a solver take (see Flags), each with the atom stating it in a theory ([value])

/** The profile of a theory (graphBuildMode): the flags its definition needs, preset */
@JsExport
enum class Profile(
    val value: String,
) {
    /** No preset: every flag as the solver and the theory state it */
    FREE("free"),

    /** ASPIC+, its preferences on the last or weakest link (the default) */
    ASPIC_PLUS("aspic_plus"),

    /** ASPIC+ with no preferences */
    ASPIC("aspic"),

    /** ASPIC+ on defeasible rules alone */
    ASPIC_DEFEASIBLE("aspic_defeasible"),

    /** The rules and the conflicts are arguments as well */
    META("meta"),

    /** Assumption-based argumentation, flat */
    ABA("aba"),

    /** ABA+, flat, under weak contraposition */
    ABA_PLUS("aba_plus"),

    /** Abstract argumentation frameworks */
    ABSTRACT("abstract"),
}

/** The semantics of the argument labelling (argumentLabellingMode) */
@JsExport
enum class Semantics(
    val value: String,
) {
    CONFLICT_FREE("conflictfree"),
    ADMISSIBLE("admissible"),
    STRONGLY_ADMISSIBLE("stronglyadmissible"),
    GROUNDED("grounded"),
    COMPLETE("complete"),
    PREFERRED("preferred"),
    SEMI_STABLE("semistable"),
    STABLE("stable"),
    IDEAL("ideal"),
    EAGER("eager"),
    NAIVE("naive"),
    STAGE("stage"),
    CF2("cf2"),
    STAGE2("stage2"),

    /** The grounded labelling with burdens of persuasion */
    BP_GROUNDED("bp_grounded"),
    BP_GROUNDED_PARTIAL("bp_grounded_partial"),
    BP_GROUNDED_COMPLETE("bp_grounded_complete"),
}

/** The labelling of the statements from that of the arguments (statementLabellingMode) */
@JsExport
enum class StatementLabelling(
    val value: String,
) {
    STATEMENT("statement"),
    STATEMENT_PASS_THROUGH("statement_pass_through"),
    STATEMENT_BINARY("statement_binary"),
}

/** The ordering principle of the preferences (orderingPrinciple) */
@JsExport
enum class OrderingPrinciple(
    val value: String,
) {
    LAST("last"),
    WEAKEST("weakest"),
}

/** The comparator of the sets of rules or premises (orderingComparator) */
@JsExport
enum class OrderingComparator(
    val value: String,
) {
    ELITIST("elitist"),
    DEMOCRAT("democrat"),
    NORMAL("normal"),
}

/** An extension of the graph (graphExtension) */
@JsExport
enum class GraphExtension(
    val value: String,
) {
    REBUT_RESTRICTION("rebutRestriction"),

    /** Burdens of persuasion */
    BP("bp"),
    STANDARD_PREF("standardPref"),
    DEFEASIBLE_PREF("defeasiblePref"),
    DEFEASIBLE_ALL_PREF("defeasibleAllPref"),

    /** The preferences of ABA+, reversing attacks */
    ABA_PREF("abaPref"),

    /** Statements over regions of numbers (from(T), until(T), between(A, B)), restricted to the numbers within them */
    CONSTRAINTS("constraints"),
}

/** A language extending the one of arg2p (graphLanguage) */
@JsExport
enum class GraphLanguage(
    val value: String,
) {
    ASPIC_LANGUAGE("aspicLanguage"),
    ABA_LANGUAGE("abaLanguage"),
    DEFEASIBLE_LANGUAGE("defeasibleLanguage"),
    ABSTRACT_LANGUAGE("abstractLanguage"),
}

/** The module building the graph (graphBuilder): from the rules, or from the arguments and attacks of the theory */
@JsExport
enum class GraphBuilder(
    val value: String,
) {
    RULE_GRAPH("ruleGraph"),
    ABSTRACT_GRAPH("abstractGraph"),
}

/**
 * A part of the conflicts beyond those of the languages of a theory (conflictModel): the flag takes any of them, all
 * by default
 */
@JsExport
enum class ConflictModel(
    val value: String,
) {
    /** A literal and its negation */
    NEGATION("negation"),

    /** The obligations and permissions in conflict: o(a) and o(-a), p(a) and o(-a), o(a) and p(-a), o(a) and -p(a) */
    DEONTIC("deontic"),

    /** A superiority and its converse: sup(X, Y) and sup(Y, X) */
    PREFERENCE("preference"),

    /** Those the theory declares (conflict/2,3): alone, as the contraries of ABA */
    DECLARED("declared"),

    /** All the parts, those built in and those declared: the default */
    BUILTIN("builtin"),
}
