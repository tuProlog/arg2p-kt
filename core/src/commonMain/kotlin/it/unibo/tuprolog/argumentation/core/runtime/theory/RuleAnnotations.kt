package it.unibo.tuprolog.argumentation.core.runtime.theory

import it.unibo.tuprolog.core.Clause
import it.unibo.tuprolog.core.operators.OperatorSet
import it.unibo.tuprolog.parser.PrologLexer
import it.unibo.tuprolog.parser.sources.SourceText
import it.unibo.tuprolog.parser.tokens.TokenKind
import it.unibo.tuprolog.theory.Theory
import it.unibo.tuprolog.theory.parsing.parse

/**
 * The identifiers a theory gives its clauses in comments: a clause followed, on the line of its full stop, by a
 * comment of a single word (`h :- b. % r5`, the clause spanning one line or more) is tagged with it ([RULE]). Under
 * prologStrictCompatibility a clause so tagged is the defeasible rule or premise of that identifier, the others
 * strict rules and axioms; otherwise the tag changes nothing. The comments are read with the lexer of 2P-Kt, which
 * keeps them, and the clauses are those its parser gives.
 */
object RuleAnnotations {
    /** The tag of a clause with its identifier */
    const val RULE = "it.unibo.tuprolog.argumentation.rule"

    // A comment of a single word, the identifier of a rule
    private val identifier = Regex("""[a-z][A-Za-z0-9_]*""")

    /** [text] parsed with [operators], its clauses tagged with the identifiers of their comments */
    fun parse(
        text: String,
        operators: OperatorSet,
    ): Theory = annotate(Theory.parse(text, operators), text)

    /** The clauses of [theory], parsed from [text], tagged with the identifiers of their comments in [text] */
    fun annotate(
        theory: Theory,
        text: String,
    ): Theory {
        val identifiers = identifiers(text)
        val clauses = theory.clauses.toList()
        // the clauses are those of the full stops: if they are not (a theory not parsed from text), nothing is tagged
        if (identifiers.all { it == null } || identifiers.size != clauses.size) return theory
        return Theory.of(
            theory.unificator,
            clauses.mapIndexed { index, clause ->
                identifiers[index]?.let { clause.replaceTags(clause.tags + (RULE to it)) as Clause } ?: clause
            },
        )
    }

    /** The identifier [clause] is tagged with, if any */
    fun of(clause: Clause): String? = clause.tags[RULE] as? String

    // The identifier of the comment after each full stop, in their order (null for a clause with none)
    private fun identifiers(text: String): List<String?> {
        val source = SourceText(text)
        val tokens =
            PrologLexer
                .default()
                .lex(source)
                .materialize()
                .tokens
                .toList()
        val identifiers = mutableListOf<String?>()
        for (stop in tokens.indices) {
            if (tokens[stop].kind != TokenKind.FULL_STOP) continue
            var next = stop + 1
            while (next < tokens.size && tokens[next].kind == TokenKind.WHITESPACE) next++
            val comment = tokens.getOrNull(next)?.takeIf { it.kind == TokenKind.LINE_COMMENT }
            identifiers +=
                comment
                    ?.takeIf { it.span.start.line == tokens[stop].span.start.line }
                    ?.let { source.text(it.span.start.offset, it.span.endExclusive.offset).removePrefix("%").trim() }
                    ?.takeIf { identifier.matches(it) }
        }
        return identifiers
    }
}
