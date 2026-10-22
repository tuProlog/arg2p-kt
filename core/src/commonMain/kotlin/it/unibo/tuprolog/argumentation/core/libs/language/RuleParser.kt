package it.unibo.tuprolog.argumentation.core.libs.language

import it.unibo.tuprolog.argumentation.core.libs.ArgsFlag
import it.unibo.tuprolog.argumentation.core.libs.PrologModule
import it.unibo.tuprolog.core.operators.Operator
import it.unibo.tuprolog.core.operators.OperatorSet
import it.unibo.tuprolog.core.operators.Specifier
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.primitive.Primitive

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
            transposition::transpose(StrictRules, StrictRules, CtrRules),
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



        rule(Id, PP, [Conclusion]) :- context_check(clause(rl(Conclusion), [Id, Premises, Conclusion])), tuple_to_list(Premises, P), ff(P, PP).
        premise(Id, [Conclusion]) :- context_check(clause(rl(Conclusion), [Id, Conclusion])).

        % candidates(+Statement, +Limit, -Count): the number of rules and premises concluding Statement, counted up to
        % Limit (an integer, or anything else for no limit)
        candidates(Statement, Limit, Count) :- context_count(clause(rl(Statement), _), Limit, Count).


        check_strict(Id) :- context_check(strict(Id)).

        prolog([prolog(Term)], Term).
        contrary([~(Term)], Term).

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
