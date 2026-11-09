package it.unibo.tuprolog.argumentation.lsp

import it.unibo.tuprolog.argumentation.Arg2p
import it.unibo.tuprolog.argumentation.Arg2pException
import it.unibo.tuprolog.argumentation.Flags
import it.unibo.tuprolog.argumentation.core.libraries.evaluation.phases.flags.ArgumentLabellingMode
import it.unibo.tuprolog.argumentation.core.libraries.evaluation.phases.flags.StatementLabellingMode
import it.unibo.tuprolog.argumentation.core.libraries.evaluation.statementAcceptance.flags.QueryMode
import it.unibo.tuprolog.argumentation.core.libraries.framework.settings.flags.GraphBuildMode

/**
 * The solvers of the servers, and what they tell of a theory without solving it: stopped past a time limit (see
 * [Deadline]) or when asked (see [PageEngine]).
 */
internal object Solvers {
    /** The query solved when none is given: the statements labelled in, out and und */
    const val DEFAULT_QUERY = "arg2p::solve(In, Out, Und)"

    // The flags shown among those a theory is solved with when none are given
    private val SHOWN = listOf(ArgumentLabellingMode, StatementLabellingMode, QueryMode)

    /**
     * The flags a theory is solved with when none are given, as facts stating their defaults: the semantics, the
     * statement labelling and the query mode, as the web playground starts with them (see links.ts there); the others
     * have their defaults unstated, and would only lengthen the flags the user is shown and the link to the playground
     */
    val DEFAULT_FLAGS: String =
        SHOWN.joinToString("") { flag ->
            if (flag.isSwitch) "${flag.predicate()}.\n" else "${flag.predicate()}(${flag.default()}).\n"
        }

    /** A solver of [theory] with [flags], what it prints going to [output] (nowhere otherwise) */
    fun of(
        theory: String,
        flags: Flags,
        output: (String) -> Unit = {},
    ): Arg2p =
        Arg2p
            .builder()
            .theory(theory)
            .flags(flags)
            .output(output)
            .build()

    /** The profile of the theory of [solver] (the theory states it, or the flags, or it is the default): none if it cannot tell */
    fun profileOf(solver: Arg2p): String =
        try {
            solver.setting(GraphBuildMode.predicate()).firstOrNull() ?: ""
        } catch (_: Arg2pException) {
            ""
        }

    /** Where [text], a theory (or flags) or a query ([kind]), does not parse; null if it does */
    fun check(
        text: String,
        kind: String,
    ): ProblemData? =
        if (kind == "query") {
            if (text.isBlank()) null else Arg2p.checkQuery(text)?.let { ProblemData.of(it) }
        } else {
            Arg2p.checkTheory(text)?.let { ProblemData.of(it) }
        }

    /** The values of the flags in effect for the theory of [solver]: an [Arg2pException] if its profile refuses it */
    fun settingsOf(solver: Arg2p): Map<String, List<String>> = solver.knownFlags.associateWith { solver.setting(it) }

    /**
     * The values of the flags in effect for [theory] solved with [flags], as the editor of a theory asks them while it
     * is written: those of the flags alone when the theory does not parse, none when they do not, none of a flag the
     * solver refuses
     */
    fun settings(
        theory: String,
        flags: String,
    ): Map<String, List<String>> {
        val parsed =
            try {
                Flags.parse(flags)
            } catch (_: Arg2pException) {
                return emptyMap()
            }
        val solver =
            try {
                of(theory, parsed)
            } catch (_: Arg2pException) {
                of("", parsed)
            }
        return solver.knownFlags.associateWith { flag ->
            try {
                solver.setting(flag)
            } catch (_: Arg2pException) {
                emptyList()
            }
        }
    }

    /** What a text to check is: a theory (or flags), or a query */
    val KINDS = listOf("theory", "query")
}
