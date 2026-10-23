package it.unibo.tuprolog.argumentation.core.modules.language

import it.unibo.tuprolog.argumentation.core.flags.ArgsFlag
import it.unibo.tuprolog.argumentation.core.flags.AutoTransposition
import it.unibo.tuprolog.argumentation.core.flags.MetaConflicts
import it.unibo.tuprolog.argumentation.core.flags.MetaRules
import it.unibo.tuprolog.argumentation.core.flags.PrologStrictCompatibility
import it.unibo.tuprolog.argumentation.core.modules.PrologModule
import it.unibo.tuprolog.argumentation.core.primitives.language.BpsNew
import it.unibo.tuprolog.argumentation.core.primitives.language.ExtractId
import it.unibo.tuprolog.argumentation.core.primitives.language.PrologAxioms
import it.unibo.tuprolog.argumentation.core.primitives.language.PrologDefeasibleRules
import it.unibo.tuprolog.argumentation.core.primitives.language.PrologPremises
import it.unibo.tuprolog.argumentation.core.primitives.language.PrologStrictRules
import it.unibo.tuprolog.argumentation.core.primitives.language.RuleToClause
import it.unibo.tuprolog.argumentation.core.primitives.language.RuleToClauseNatural
import it.unibo.tuprolog.core.operators.Operator
import it.unibo.tuprolog.core.operators.OperatorSet
import it.unibo.tuprolog.core.operators.Specifier
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.primitive.Primitive

/**
 * The rules of a theory, read by the graph builders (module `parser`): convertAllRules/1 converts the rules,
 * premises, preferences and burdens of the theory, which classic_rule/2,3 and the other predicates read.
 */
object ParserModule : PrologModule("prolog.argumentation.parser", "parser") {
    override val primitives: Map<Signature, Primitive>
        get() =
            mapOf(
                PrologStrictRules::descriptionPair.get(),
                PrologAxioms::descriptionPair.get(),
                BpsNew::descriptionPair.get(),
                PrologPremises::descriptionPair.get(),
                PrologDefeasibleRules::descriptionPair.get(),
                ExtractId::descriptionPair.get(),
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

    fun operators() =
        OperatorSet(
            Operator("=>", Specifier.XFX, 1199),
            Operator(":=>", Specifier.XFX, 1199),
            Operator(":->", Specifier.XFX, 1199),
            Operator(":", Specifier.XFX, 1001),
            Operator(":=", Specifier.XFX, 1199),
//            Operator("->", Specifier.XFX, 1050)
        )

    override val prologRawTheory: String =
        """
        /*
         *  Main directive. Find all the rules in the theory in the format (Name : Preconditions => Conclusion)
         *  or (Name : Effects) and then if:
         *  - It is a standard rule, translate it in the standard format (rule([Name, Preconditions, Conclusions]));
         *  - It is a bp rule, translate it to (abstractBp([Effects]))
         *  During the process, clean them from the unallowed symbols (-, o, p).
         */
        %! convertAllRules(-Rules)
        %  The rules, premises, preferences and burdens of the theory, in the formats the graph builders read (rl/1 clauses, strict/1, sup/2, abstractBp/1), asserted; Rules are what was asserted.
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

        recoverId(Head :- Body, Id) :- rule::rule(Id, _, _, Head, Body).
        recoverId(Head :- Body, Id) :- rule::premise(Id, _, Head, Body).
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



        %! rule(?Id, ?Premises, ?Conclusion)
        %  The rule Id as converted (see convertAllRules/1), concluding Conclusion from Premises.
        rule(Id, PP, [Conclusion]) :-
            rule::rule(Id, Premises, Conclusion, Head, Body),
            context_check(clause(Head, Body)),
            tuple_to_list(Premises, P),
            ff(P, PP).
        premise(Id, [Conclusion]) :-
            rule::premise(Id, Conclusion, Head, Body),
            context_check(clause(Head, Body)).

        %! candidates(+Statement, +Limit, -Count)
        %  The number of rules and premises concluding Statement, counted up to
        %  Limit (an integer, or anything else for no limit)
        candidates(Statement, Limit, Count) :-
            rule::byConclusion(Statement, Head),
            context_count(clause(Head, _), Limit, Count).


        %! check_strict(?Id)
        %  The rule or premise Id is strict.
        check_strict(Id) :- context_check(strict(Id)).

        %! prolog(?Premise, ?Goal)
        %  Premise, as [prolog(Goal)], is a Prolog check.
        prolog([prolog(Term)], Term).
        %! contrary(?Premise, ?Statement)
        %  Premise, as [~(Statement)], is a contrary premise.
        contrary([~(Term)], Term).

        /*
         *   Convert the given tuple to list
         */
        %! tuple_to_list(+Conjunction, -List)
        %  The items of a conjunction, as a list (built with an accumulator rather than appending the lists of the two
        %  sides, which took a module call at every step)
        tuple_to_list(T, L) :- tuple_to_list(T, L, []).
        tuple_to_list(A, [A|R], R) :- var(A), !.
        tuple_to_list((A, B), L, R) :- !, tuple_to_list(A, L, M), tuple_to_list(B, M, R).
        tuple_to_list(A, [A|R], R).

        ff([[]], []) :- !.
        ff([[X]], [X]) :- !.
        ff(X, X).

        %! classic_rule(?Id, ?Premises, ?Conclusion)
        %  The rule Id concludes Conclusion ([Statement]) from Premises (a list); with metaRules, applicable(Id) is its first premise.
        %  classic_rule(mc, [conflict(A, B), A], [A]) :- metaConflicts.
        classic_rule(X, [applicable(X)|Y], Z) :- setting(metaRules), !, rule(X, Y, Z).
        classic_rule(X, Y, Z) :- rule(X, Y, Z).
        %! classic_rule(?Id, ?Conclusion)
        %  The premise Id concludes Conclusion ([Statement]).
        classic_rule(X, Y) :- premise(X, Y).

        %! ruleUsing(?Premise, ?Conclusion)
        %  A rule concludes Conclusion (a statement) from Premise, among others: classic_rule/3 and member/2 over its
        %  premises, through an index of the premises of the rules built once in the active context.
        ruleUsing(Premise, Conclusion) :-
            premisesIndexed,
            context_check(premiseOf(Premise, Conclusion)).

        % The index: premiseOf/2 facts asserted from the last, as context_check returns the newest clauses first, so
        % that lookups give the rules in the order of classic_rule/3
        premisesIndexed :- context_check(premisesIndexed), !.
        premisesIndexed :-
            findall(premiseOf(P, C), (classic_rule(_, Premises, [C]), member(P, Premises)), Index),
            assertFromLast(Index),
            context_assert(premisesIndexed).

        assertFromLast([]).
        assertFromLast([Fact|Facts]) :-
            assertFromLast(Facts),
            context_assert(Fact).
        """.trimIndent()
}
