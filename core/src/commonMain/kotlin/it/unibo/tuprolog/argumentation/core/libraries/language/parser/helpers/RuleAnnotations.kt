package it.unibo.tuprolog.argumentation.core.libraries.language.parser.helpers

import it.unibo.tuprolog.argumentation.engine.words.locked
import it.unibo.tuprolog.core.Clause
import it.unibo.tuprolog.core.operators.OperatorSet
import it.unibo.tuprolog.parser.PrologLexer
import it.unibo.tuprolog.parser.sources.SourceText
import it.unibo.tuprolog.parser.tokens.TokenKind
import it.unibo.tuprolog.theory.Theory
import it.unibo.tuprolog.theory.parsing.parseAsClausesLazily
import it.unibo.tuprolog.unify.Unificator

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
    ): Theory = Theory.of(Unificator.default, clauses(text, operators))

    /**
     * The theory of [text] for a solver of [unificator] reading its clauses, which the machine does by its own index: listed, with
     * no index of it to build for each solver
     */
    fun parse(
        text: String,
        operators: OperatorSet,
        unificator: Unificator,
    ): Theory = Theory.listedOf(unificator, clauses(text, operators))

    // The clauses of [text]: read off the text, as those of an indexed theory take a time growing with their square to read
    private fun clauses(
        text: String,
        operators: OperatorSet,
    ): List<Clause> {
        // (the same text comes again and again from a server or a page, and reading it is the largest part of a first solve)
        val known = locked(parsed) { parsed.firstOrNull { it.text == text && it.operators == operators } }
        if (known != null) return known.clauses
        val clauses = tagged(text.parseAsClausesLazily(operators).toList(), text)
        if (text.length <= PARSED_TEXT_LIMIT) {
            locked(parsed) {
                if (parsed.size >= PARSED_KEPT) parsed.removeAt(0)
                parsed.add(Parsed(text, operators, clauses))
            }
        }
        return clauses
    }

    private class Parsed(
        val text: String,
        val operators: OperatorSet,
        val clauses: List<Clause>,
    )

    // The texts parsed last, the terms of a clause being immutable: a theory is made of them for each solver
    private val parsed = ArrayList<Parsed>()
    private const val PARSED_KEPT = 8
    private const val PARSED_TEXT_LIMIT = 1 shl 20

    // [clauses], parsed from [text], tagged with the identifiers of their comments
    private fun tagged(
        clauses: List<Clause>,
        text: String,
    ): List<Clause> {
        // (the identifiers are in comments: with none, the text need not be read again)
        if (!mayHaveIdentifiers(text)) return clauses
        val identifiers = identifiers(text)
        // the clauses are those of the full stops: if they are not (a theory not parsed from text), nothing is tagged
        if (identifiers.all { it == null } || identifiers.size != clauses.size) return clauses
        return clauses.mapIndexed { index, clause ->
            identifiers[index]?.let { clause.replaceTags(clause.tags + (RULE to it)) as Clause } ?: clause
        }
    }

    // Whether a comment after a full stop may be an identifier (a line with no such comment needs no lexer)
    private fun mayHaveIdentifiers(text: String): Boolean {
        var at = text.indexOf('%')
        while (at >= 0) {
            var before = at - 1
            while (before >= 0 && (text[before] == ' ' || text[before] == '\t')) before--
            if (before >= 0 && text[before] == '.') {
                val end = text.indexOf('\n', at).let { if (it < 0) text.length else it }
                if (identifier.matches(text.substring(at + 1, end).trim())) return true
            }
            at = text.indexOf('%', at + 1)
        }
        return false
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
