package it.unibo.tuprolog.argumentation

import it.unibo.tuprolog.core.Atom
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.primitive.Primitive

/**
 * The spellings of the versions of arg2p before 0.26.0, as the papers on it write them, read as those of 0.26.0: the values
 * of the flags ([values], the modules of the semantics named as them), and the names of the predicates ([predicates]). A
 * theory, a goal or flags written with them run as they did; arg2p itself writes the names of 0.26.0 alone (in camelCase),
 * and the language server marks an earlier spelling with the one to write.
 */
object EarlierSpellings {
    /** The values of the flags, by their earlier spelling: the names of the modules of the semantics too */
    val values: Map<String, String> =
        mapOf(
            "aspic_plus" to "aspicPlus",
            "aspic_defeasible" to "aspicDefeasible",
            "aba_plus" to "abaPlus",
            "standard_af" to "aspicPlus",
            "conflictfree" to "conflictFree",
            "stronglyadmissible" to "stronglyAdmissible",
            "semistable" to "semiStable",
            "grounded_hash" to "grounded",
            "bp_grounded" to "bpGrounded",
            "bp_grounded_partial" to "bpGroundedPartial",
            "bp_grounded_complete" to "bpGroundedComplete",
            "statement_pass_through" to "statementPassThrough",
            "statement_binary" to "statementBinary",
            "whole_graph" to "wholeGraph",
        )

    /** The names of the predicates, by their earlier spelling (the arities are the same) */
    val predicates: Map<String, String> =
        listOf(
            "but_for",
            "ness_original",
            "ness_intervention",
            "ness_original_intervention",
            "call_module",
            "module_defines",
            "extension_flag",
            "linear_constraint",
            "linear_satisfiable",
            "cache_assert",
            "cache_check",
            "cache_count",
            "cache_retract",
            "context_absent",
            "context_active",
            "context_assert",
            "context_assert_id",
            "context_branch",
            "context_by",
            "context_check",
            "context_checkout",
            "context_count",
            "context_drop",
            "context_drop_after",
            "context_id",
            "context_last",
            "context_matching",
            "context_reset",
            "context_retract",
            "context_retract_all",
            "context_stored_id",
            "context_term",
            "graph_attack_saved",
            "graph_begin",
            "graph_explore",
            "graph_own",
            "graph_removed",
            "graph_settle",
            "graph_share",
            "graph_step",
            "graph_sync",
        ).associateWith { camel(it) }

    /** The spelling of 0.26.0 of [name], a value, a module or a predicate written as an earlier version did; null otherwise */
    fun current(name: String): String? = values[name] ?: predicates[name]

    /** [value], a value of a flag, as 0.26.0 spells it */
    fun value(value: Term): Term = (value as? Atom)?.value?.let { values[it] }?.let { Atom.of(it) } ?: value

    // The earlier spellings of each predicate of 0.26.0
    private val earlier: Map<String, List<String>> = predicates.entries.groupBy({ it.value }, { it.key })

    /**
     * [primitives] with each also under the earlier spelling of its name, if it has one: the same primitive, called by
     * either name (the machine finds a primitive by its name, the cost of the other none)
     */
    fun withEarlierNames(primitives: Map<Signature, Primitive>): Map<Signature, Primitive> {
        if (primitives.keys.none { it.name in earlier }) return primitives
        val all = LinkedHashMap(primitives)
        for ((signature, primitive) in primitives) {
            earlier[signature.name]?.forEach { all[Signature(it, signature.arity, signature.vararg)] = primitive }
        }
        return all
    }

    /** Whether [signature] is the earlier spelling of a predicate, not one of 0.26.0 */
    fun isEarlier(signature: Signature): Boolean = signature.name in predicates

    private fun camel(name: String): String {
        val parts = name.split('_')
        return parts.first() + parts.drop(1).joinToString("") { it.replaceFirstChar(Char::uppercaseChar) }
    }
}
