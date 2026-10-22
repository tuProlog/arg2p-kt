package it.unibo.tuprolog.argumentation.core.libs.language

import it.unibo.tuprolog.argumentation.core.libs.ArgsFlag
import it.unibo.tuprolog.argumentation.core.libs.PrologModule
import it.unibo.tuprolog.argumentation.core.libs.basic.Settings
import it.unibo.tuprolog.core.Clause
import it.unibo.tuprolog.core.List
import it.unibo.tuprolog.core.Struct
import it.unibo.tuprolog.core.Substitution
import it.unibo.tuprolog.core.Term
import it.unibo.tuprolog.core.operators.Operator
import it.unibo.tuprolog.core.operators.OperatorSet
import it.unibo.tuprolog.core.operators.Specifier
import it.unibo.tuprolog.core.parsing.parse
import it.unibo.tuprolog.core.toTerm
import it.unibo.tuprolog.dsl.solve.LogicProgrammingScope
import it.unibo.tuprolog.dsl.solve.prolog
import it.unibo.tuprolog.solve.ExecutionContext
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.extractSignature
import it.unibo.tuprolog.solve.primitive.BinaryRelation
import it.unibo.tuprolog.solve.primitive.Primitive
import it.unibo.tuprolog.solve.primitive.PrimitiveWrapper.Companion.ensuringArgumentIsVariable
import it.unibo.tuprolog.solve.primitive.Solve
import it.unibo.tuprolog.solve.primitive.UnaryPredicate
import kotlin.collections.List as KList

abstract class RuleParserBase : PrologModule("prolog.argumentation.parser", "parser") {
    override val primitives: Map<Signature, Primitive>
        get() =
            mapOf(
                StrictRules::descriptionPair.get(),
                Axioms::descriptionPair.get(),
                Bps::descriptionPair.get(),
                Premises::descriptionPair.get(),
                DefeasibleRules::descriptionPair.get(),
                ExtractStrictIds::descriptionPair.get(),
                RuleToClause::descriptionPair.get(),
                RuleToClauseNatural::descriptionPair.get(),
            )

    // The operators of the rules of a theory, for the theory of the parser and the solvers using it
    override val libraryOperators: OperatorSet
        get() = operators()

    override val theoryOperators: OperatorSet
        get() = super.theoryOperators.plus(operators())

    override val baseFlags: Iterable<ArgsFlag<*, *>>
        get() = listOf(AutoTransposition, PrologStrictCompatibility, MetaConflicts, MetaRules)

    companion object {
        fun operators() =
            OperatorSet(
                Operator("=>", Specifier.XFX, 1199),
                Operator(":=>", Specifier.XFX, 1199),
                Operator(":->", Specifier.XFX, 1199),
                Operator(":", Specifier.XFX, 1001),
                Operator(":=", Specifier.XFX, 1199),
//            Operator("->", Specifier.XFX, 1050)
            )
    }
}

object RuleParser : RuleParserBase() {
    override val prologRawTheory: String =
        """
        /*
         *  Main directive. Find all the rules in the theory in the format (Name : Preconditions => Conclusion)
         *  or (Name : Effects) and then if:
         *  - It is a standard rule, translate it in the standard format (rule([Name, Preconditions, Conclusions]));
         *  - It is a bp rule, translate it to (abstractBp([Effects]))
         *  During the process, clean them from the unallowed symbols (-, o, p).
         */
        convertAllRules(ArgRules) :-
            defeasibleRules(DefeasibleRules),
            strictRules(StrictRules, RulesIds),
            ordinaryPremises(Premises),
            axiomPremises(Axioms, AxiomsIds),
            specialRules(SpecialRules),
            utils::appendLists([AxiomsIds, RulesIds], StrictIds),
            utils::appendLists([DefeasibleRules, StrictRules, Premises, Axioms], Rules),
            naturalRules(Rules, Clauses, StrictIds, MappedIds),
            findall(sup(X, Y), sup(X, Y), Sup),
            utils::appendLists([Clauses, MappedIds, Sup, SpecialRules], ArgRules),
            utils::assert_all(ArgRules), !.

        naturalRules(Rules, Clauses, StrictIds, MappedIds) :-
            naturalTerms,
            rule_to_clause_natural(Rules, Clauses),
            mapIds(Rules, Clauses, StrictIds, MappedIds).
        naturalRules(Rules, Clauses, StrictIds, StrictIds) :-
            \+ naturalTerms,
            rule_to_clause(Rules, Clauses).

        recoverId(rl(_) :- [Y, _, _], Y).
        recoverId(rl(_) :- [Y, _], Y).
        recoverId([Y, _, _], Y).
        recoverId([Y, _], Y).

        mapIds([], [], _, []).
        mapIds([W|T], [Z|TT], Ids, [strict(Y)|RR]) :-
            once(recoverId(W, X)),
            member(strict(X), Ids), !,
            once(recoverId(Z, Y)),
            mapIds(T, TT, Ids, RR).
        mapIds([_|T], [_|TT], Ids, RR) :-
            mapIds(T, TT, Ids, RR).

        defeasibleRules(DefeasibleRules) :-
            findall([RuleName, Preconditions, Effect], (RuleName : Preconditions => Effect), DefeasibleRulesOld),
            prologDefeasibleRules(DefeasibleRulesNew),
            utils::append_fast(DefeasibleRulesOld, DefeasibleRulesNew, DefeasibleRules).

        strictRules(CtrRules, Ids) :-
            findall([RuleName, Preconditions, Effect], (RuleName : Preconditions -> Effect), StrictRulesOld),
            prologStrictRules(StrictRulesNew),
            utils::append_fast(StrictRulesOld, StrictRulesNew, StrictRules),
            transpose(StrictRules, StrictRules, CtrRules),
            extract_id(CtrRules, Ids).

        ordinaryPremises(Premises) :-
            findall([RuleName, Effect], ((RuleName :=> Effect), atom(RuleName)), PremisesOld),
            prologPremises(PremisesNew),
            utils::append_fast(PremisesOld, PremisesNew, Premises).

        axiomPremises(Axioms, Ids) :-
            findall([RuleName, Effect], ((RuleName :-> Effect), atom(RuleName)), AxiomsOld),
            prologAxioms(AxiomsNew),
            utils::append_fast(AxiomsOld, AxiomsNew, Axioms),
            extract_id(Axioms, Ids).

        specialRules(SpecialRules) :-
            bpsNew(SpecialRules).
        %    findall([bps, X], search('bp', 10, X), SpecialRules).

        %=======================================================================================================================
        % TRANSPOSITION
        %=======================================================================================================================

        transpose(_, CtrRules, CtrRules) :- \+ setting(autoTransposition), !.
        % The rules followed by the transpositions of each one, from the last rule to the first, keeping the last rule
        % with each name: what merging the transpositions of a rule at a time, and deduplicating each time, amounts to,
        % in a single pass rather than merging and deduplicating the lists once for each rule (cubic in the number of
        % rules).
        transpose(Rules, TempCtrRules, CtrRules) :-
            reverse(Rules, Reversed),
            findall(TrH, (member(H, Reversed), transposition(H, TrH)), Tran),
            utils::append_fast(TempCtrRules, Tran, All),
            utils::unique_by_first(All, CtrRules).

        transposition([Id, Prec, Effect], [Id, Prec, Effect]).
        transposition([Id, Prec, Effect], [NewId, NewPrec ,XNegated]) :-
            compound(Prec),
            tuple_to_list(Prec, LPrec),!,
            transposition_sequential(LPrec, LPrec, Effect, Id, [], NewPrec, XNegated, NewId).
        transposition([Id, Prec, Effect], [NewId, EffectNegated ,XNegated]) :-
            \+ compound(Prec),
            Prec \== [],
            prologEscape(Prec),
            negate(Prec, XNegated),
            negate(Effect, EffectNegated),
            atom_concat(Id, '_i', NewId).

        transposition_sequential(LPrec, [H|T], Effect, Id, Skipped, NewPrec, XNegated, NewId) :-
            transposition_sequential(LPrec, T, Effect, Id, [H|Skipped], NewPrec, XNegated, NewId).
        transposition_sequential(LPrec, [X|T], Effect, Id, Skipped, NewPrec, XNegated, NewId) :-
            prologEscape(X),
            utils::append_fast(Skipped, T, CleanedPrec),
            negate(X, XNegated),
            negate(Effect, EffectNegated),
            list_to_tuple([EffectNegated|CleanedPrec], NewPrec),
            newIdentifier(Skipped, Id, NewId).

        newIdentifier(List, OldId, NewId) :-
            modifier(List, Mod),
            atom_concat(OldId, Mod, NewId).

        modifier([], '_i').
        modifier([_|Tail], Index):-
            modifier(Tail, Index1),
            atom_concat(Index1, 'i', Index).

        negate(X, Arg) :-
            functor(X, '-', _) -> (
                arg(1, X, Arg));
            functor(X, 'p', _) -> (
                arg(1, X, PX),
                (functor(PX, '-', _) -> (
                    arg(1, PX, PNX),
                    Arg = o(PNX)
                );
                Arg = o(-(PX))));
            Arg = -(X).

        prologEscape(X) :- \+ functor(X, 'prolog', _), \+ functor(X, '~', _).

        %=======================================================================================================================

        list_to_tuple([H], (H)) :- !.
        list_to_tuple([H|T], (H,TT)) :- list_to_tuple(T,TT).


        rule(Id, PP, [Conclusion]) :- context_check(clause(rl(Conclusion), [Id, Premises, Conclusion])), tuple_to_list(Premises, P), ff(P, PP).
        premise(Id, [Conclusion]) :- context_check(clause(rl(Conclusion), [Id, Conclusion])).

        % candidates(+Statement, +Limit, -Count): the number of rules and premises concluding Statement, counted up to
        % Limit (an integer, or anything else for no limit)
        candidates(Statement, Limit, Count) :- context_count(clause(rl(Statement), _), Limit, Count).


        check_strict(Id) :- context_check(strict(Id)).

        prolog([prolog(Term)], Term).
        contrary([~(Term)], Term).
        undercut(undercut(Rule), Rule).
        superiority(sup(Id1, Id2), Id1, Id2).

        /*
         *   Convert the given tuple to list
         */
        % The items of a conjunction, as a list (built with an accumulator rather than appending the lists of the two
        % sides, which took a module call at every step)
        tuple_to_list(T, L) :- tuple_to_list(T, L, []).
        tuple_to_list(A, [A|R], R) :- var(A), !.
        tuple_to_list((A, B), L, R) :- !, tuple_to_list(A, L, M), tuple_to_list(B, M, R).
        tuple_to_list(A, [A|R], R).

        ff([[]], []) :- !.
        ff([[X]], [X]) :- !.
        ff(X, X).

        % classic_rule(mc, [conflict(A, B), A], [A]) :- metaConflicts.
        classic_rule(X, [applicable(X)|Y], Z) :- setting(metaRules), !, rule(X, Y, Z).
        classic_rule(X, Y, Z) :- rule(X, Y, Z).
        classic_rule(X, Y) :- premise(X, Y).
        """.trimIndent()
}

object AutoTransposition : ArgsFlag<Boolean, Unit> {
    override fun predicate(): String = "autoTransposition"

    override val isSwitch: Boolean = true

    override fun default(): Boolean = false

    override fun values() {}
}

object PrologStrictCompatibility : ArgsFlag<Boolean, Unit> {
    override fun predicate(): String = "prologStrictCompatibility"

    override val isSwitch: Boolean = true

    override fun default(): Boolean = false

    override fun values() {}
}

// Conflicts derived as statements of the theory (conflict/2,3 arguments), rather than built in
object MetaConflicts : ArgsFlag<Boolean, Unit> {
    override fun predicate(): String = "metaConflicts"

    override val isSwitch: Boolean = true

    override fun default(): Boolean = false

    override fun values() {}
}

// Rules applicable only when an applicable(Rule) statement holds
object MetaRules : ArgsFlag<Boolean, Unit> {
    override fun predicate(): String = "metaRules"

    override val isSwitch: Boolean = true

    override fun default(): Boolean = false

    override fun values() {}
}

object ConversionUtils {
    fun modifiers(
        target: Clause,
        context: Solve.Request<ExecutionContext>,
    ): Term =
        prolog {
            target.bodyItems
                .map { term ->
                    if (term.isStruct && term.asStruct()?.functor == "\\+") {
                        "~"(term.asStruct()!!.args[0])
                    } else if (isPrologGoal(term, context.context)) {
                        "prolog"(term)
                    } else {
                        term
                    }
                }.let { if (it.count() > 1) tupleOf(it) else it.first() }
        }

    /**
     * Whether a goal of a rule is a Prolog goal: one clause/2 cannot inspect (a variable, a term that is not
     * callable, or a procedure the libraries protect), rather than a statement of the theory
     */
    private fun isPrologGoal(
        goal: Term,
        context: ExecutionContext,
    ): Boolean = goal !is Struct || context.libraries.hasProtected(goal.extractSignature())

    /** The identifier of the rule a Prolog clause stands for: from its position in the theory, the same at each conversion */
    fun ruleId(index: Int): String = "rule_$index"

    fun commonMap(
        context: Solve.Request<ExecutionContext>,
        first: Term,
        force: Boolean = false,
        mapper: (Iterable<IndexedValue<Clause>>, LogicProgrammingScope) -> Term,
    ): Sequence<Substitution> {
        context.ensuringArgumentIsVariable(0)
        return prolog {
            sequenceOf(
                Substitution.of(
                    first.castToVar(),
                    if (force || Settings.of(context.context)?.isOn(PrologStrictCompatibility, context.context) == true) {
                        mapper(
                            context.context.staticKb.clauses
                                .withIndex(),
                            this,
                        )
                    } else {
                        emptyLogicList
                    },
                ),
            )
        }
    }
}

object StrictRules : UnaryPredicate.WithoutSideEffects<ExecutionContext>("prologStrictRules") {
    override fun Solve.Request<ExecutionContext>.computeAllSubstitutions(first: Term): Sequence<Substitution> =
        ConversionUtils.commonMap(this, first) { clauses, prologScope ->
            clauses
                .filter { !it.value.isFact }
                .map { (index, clause) ->
                    prologScope.logicListOf(
                        ConversionUtils.ruleId(index),
                        ConversionUtils.modifiers(clause, this@computeAllSubstitutions),
                        clause.head!!,
                    )
                }.toTerm()
        }
}

object Axioms : UnaryPredicate.WithoutSideEffects<ExecutionContext>("prologAxioms") {
    override fun Solve.Request<ExecutionContext>.computeAllSubstitutions(first: Term): Sequence<Substitution> =
        ConversionUtils.commonMap(this, first) { clauses, prologScope ->
            clauses
                .filter { it.value.isFact && !listOf(":->", "->", "=>", ":=>", ":", ":=", ",").contains(it.value.head!!.functor) }
                .map { (index, clause) -> prologScope.logicListOf(ConversionUtils.ruleId(index), clause.head!!) }
                .toTerm()
        }
}

object Premises : UnaryPredicate.WithoutSideEffects<ExecutionContext>("prologPremises") {
    override fun Solve.Request<ExecutionContext>.computeAllSubstitutions(first: Term): Sequence<Substitution> =
        ConversionUtils.commonMap(this, first) { clauses, prologScope ->
            clauses
                .filter { it.value.isFact && it.value.head!!.functor == ":=" && it.value.head!!.arity == 1 }
                .map { (index, clause) -> prologScope.logicListOf(ConversionUtils.ruleId(index), clause.head!!.args[0]) }
                .toTerm()
        }
}

object DefeasibleRules : UnaryPredicate.WithoutSideEffects<ExecutionContext>("prologDefeasibleRules") {
    override fun Solve.Request<ExecutionContext>.computeAllSubstitutions(first: Term): Sequence<Substitution> =
        ConversionUtils.commonMap(this, first) { clauses, prologScope ->
            clauses
                .filter { (_, clause) ->
                    clause.isFact &&
                        (
                            clause.head!!.functor == ":=" ||
                                (
                                    clause.head!!.functor == "," &&
                                        clause.head!!
                                            .args[0]
                                            .asStruct()
                                            ?.functor == ":="
                                )
                        ) &&
                        clause.head!!.arity == 2
                }.map { (index, clause) ->
                    if (clause.head!!.functor == ",") {
                        val head = clause.head!!.args[0].asStruct()
                        val term =
                            (
                                listOf(head!![1]) + (
                                    clause.head!!
                                        .args[1]
                                        .asTuple()
                                        ?.args
                                        ?: listOf(clause.head!!.args[1])
                                )
                            ).toTypedArray()
                        prologScope.clauseOf(head[0].asStruct(), *term)
                    } else {
                        prologScope.clauseOf(clause.head!!.args[0].asStruct(), clause.head!!.args[1])
                    }.let {
                        prologScope.logicListOf(
                            ConversionUtils.ruleId(index),
                            ConversionUtils.modifiers(it, this@computeAllSubstitutions),
                            it.head!!,
                        )
                    }
                }.toTerm()
        }
}

object Bps : UnaryPredicate.WithoutSideEffects<ExecutionContext>("bpsNew") {
    override fun Solve.Request<ExecutionContext>.computeAllSubstitutions(first: Term): Sequence<Substitution> =
        ConversionUtils.commonMap(this, first, true) { clauses, prologScope ->
            clauses
                .filter { it.value.isFact && it.value.head?.functor == "bp" }
                .map { Struct.of("abstractBp", prologScope.logicListOf(it.value.head!!.args)) }
                .toTerm()
        }
}

object ExtractStrictIds : BinaryRelation.WithoutSideEffects<ExecutionContext>("extract_id") {
    override fun Solve.Request<ExecutionContext>.computeAllSubstitutions(
        first: Term,
        second: Term,
    ): Sequence<Substitution> =
        sequenceOf(
            Substitution.of(
                second.asVar()!!,
                List.of(
                    first.asList()!!.toList().map {
                        Struct.of("strict", it.asCons()!!.head)
                    },
                ),
            ),
        )
}

object RuleToClauseNatural : BinaryRelation.WithoutSideEffects<ExecutionContext>("rule_to_clause_natural") {
    class TemplateExtractor {
        private val upperRegex = Regex("""\b[A-Z][a-zA-Z]*\b""")
        private val bracketRegex = Regex("""\[([^\]]+)\]""")

        // template -> number of extracted slots
        private val knownTemplates = mutableMapOf<String, Int>()

        data class Result(
            val extracted: KList<String>,
            val normalized: String,
        )

        fun processLiteralRestricted(
            input: String,
            parser: (String) -> String,
        ): String {
            val trimmed =
                input
                    .replace("'-'", "-")
                    .replace("'~'", "~")
                    .trim()

            val regex = Regex("""^(-)?(~)?\(?(o|p)?\(?(-)?\(?([^()]+)\)?\)?\)?$""")

            val match = regex.matchEntire(trimmed) ?: return parser(trimmed)
            val atomText = match.groups[5]!!.value

            return trimmed.replace(atomText, parser(atomText))
        }

        fun process(text: String): String =
            processLiteralRestricted(text) {
                processInternal(it).normalized
            }

        fun processInternal(text: String): Result {
            val text =
                text
                    .trim()
                    .removeSurrounding("'")
                    .removeSurrounding("\"")

            if (text.contains("(")) {
                return Result(emptyList(), text)
            }

            // 1. Uppercase extraction
            val upperMatches = upperRegex.findAll(text).map { it.value }.toList()
            if (upperMatches.isNotEmpty()) {
                val predicate = buildPredicate(text, upperMatches)
                val template = buildTemplate(text, upperMatches)
                knownTemplates[template] = upperMatches.size
                return Result(upperMatches, predicate)
            }

            // 2. Bracket extraction
            val bracketMatches = bracketRegex.findAll(text).map { it.groupValues[1] }.toList()
            if (bracketMatches.isNotEmpty()) {
                val predicate = buildPredicate(text.replace("[", "").replace("]", ""), bracketMatches)
                return Result(bracketMatches, predicate)
            }

            // 3. Template matching
            for ((template, _) in knownTemplates) {
                val regex = Regex(template)
                val match = regex.matchEntire(text)
                if (match != null) {
                    val extracted = match.groupValues.drop(1)
                    val res = buildPredicate(text, extracted)
                    return Result(extracted, res)
                }
            }

            // 4. Fallback
            return Result(emptyList(), text.replace(" ", "_"))
        }

        private fun buildPredicate(
            text: String,
            extracted: KList<String>,
        ): String {
            var tmp = text

            // Remove extracted elements
            extracted.forEach {
                tmp = tmp.replaceFirst(Regex("""\b${Regex.escape(it)}\b"""), "")
            }

            // Normalize predicate
            val predicate =
                tmp
                    .trim()
                    .replace(Regex("""\s+"""), "_")

            // Build arguments
            val args = extracted.joinToString(", ")

            return "$predicate($args)"
        }

        private fun buildTemplate(
            text: String,
            extracted: KList<String>,
        ): String {
            var template = text
            extracted.forEach { template = template.replace(it, "(.+?)") }
            return "^$template$"
        }
    }

    fun parseTerm(
        extractor: TemplateExtractor,
        target: KList<Term>,
    ): KList<Term> {
        val mapped =
            target.map { term ->
                if (term.isTuple) {
                    term.asTuple()!!.toList().joinToString(",", prefix = "(", postfix = ")") {
                        extractor.process(it.toString())
                    }
                } else {
                    extractor.process(term.toString())
                }
            }
        return Term.parse("[${mapped.joinToString(",")}]").asList()!!.toList()
    }

    override fun Solve.Request<ExecutionContext>.computeAllSubstitutions(
        first: Term,
        second: Term,
    ): Sequence<Substitution> {
        val extractor = TemplateExtractor()
        return sequenceOf(
            Substitution.of(
                second.asVar()!!,
                List.of(
                    first
                        .asList()!!
                        .toList()
                        .map { original ->
                            original.asList()!!.toList().let {
                                val parsed = parseTerm(extractor, it)
                                when (it.size) {
                                    2 -> Clause.of(Struct.Companion.of("rl", parsed[1]), List.of(parsed))
                                    3 -> Clause.of(Struct.Companion.of("rl", parsed[2]), List.of(parsed))
                                    else -> throw IllegalArgumentException()
                                }
                            }
                        },
                ),
            ),
        )
    }
}

object RuleToClause : BinaryRelation.WithoutSideEffects<ExecutionContext>("rule_to_clause") {
    override fun Solve.Request<ExecutionContext>.computeAllSubstitutions(
        first: Term,
        second: Term,
    ): Sequence<Substitution> =
        sequenceOf(
            Substitution.of(
                second.asVar()!!,
                List.of(
                    first
                        .asList()!!
                        .toList()
                        .map { original ->
                            original.asList()!!.toList().let {
                                when (it.size) {
                                    2 -> Clause.of(Struct.Companion.of("rl", it[1]), original)
                                    3 -> Clause.of(Struct.Companion.of("rl", it[2]), original)
                                    else -> throw IllegalArgumentException()
                                }
                            }
                        },
                ),
            ),
        )
}
