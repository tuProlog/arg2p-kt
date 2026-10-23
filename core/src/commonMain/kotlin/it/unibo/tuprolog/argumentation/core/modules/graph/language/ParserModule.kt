package it.unibo.tuprolog.argumentation.core.modules.graph.language

import it.unibo.tuprolog.argumentation.core.flags.ArgsFlag
import it.unibo.tuprolog.argumentation.core.flags.AutoTransposition
import it.unibo.tuprolog.argumentation.core.flags.MetaConflicts
import it.unibo.tuprolog.argumentation.core.flags.MetaRules
import it.unibo.tuprolog.argumentation.core.flags.PrologStrictCompatibility
import it.unibo.tuprolog.argumentation.core.modules.PrologModule
import it.unibo.tuprolog.argumentation.core.primitives.graph.language.BpsNew
import it.unibo.tuprolog.argumentation.core.primitives.graph.language.ExtractId
import it.unibo.tuprolog.argumentation.core.primitives.graph.language.PrologAxioms
import it.unibo.tuprolog.argumentation.core.primitives.graph.language.PrologDefeasibleRules
import it.unibo.tuprolog.argumentation.core.primitives.graph.language.PrologPremises
import it.unibo.tuprolog.argumentation.core.primitives.graph.language.PrologStrictRules
import it.unibo.tuprolog.argumentation.core.primitives.graph.language.RuleToClause
import it.unibo.tuprolog.argumentation.core.primitives.graph.language.RuleToClauseNatural
import it.unibo.tuprolog.core.operators.Operator
import it.unibo.tuprolog.core.operators.OperatorSet
import it.unibo.tuprolog.core.operators.Specifier
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.primitive.Primitive

/**
 * The rules of a theory, read by the graph builders (module `parser`): convertAllRules/1 converts the rules,
 * premises, preferences and burdens of the theory, which classicRule/2,3 and the other predicates read.
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
//            Operator("->", Specifier.XFX, 1050)
        )

    override val prologRawTheory: String =
        // language=Prolog
        """
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
            utils::assertAll(ArgRules), !.

        naturalRules(Rules, Clauses, StrictIds, MappedIds) :-
            naturalTerms,
            ruleToClauseNatural(Rules, Clauses),
            mapIds(Rules, Clauses, StrictIds, MappedIds).
        naturalRules(Rules, Clauses, StrictIds, StrictIds) :-
            \+ naturalTerms,
            ruleToClause(Rules, Clauses).

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
            utils::appendFast(DefeasibleRulesOld, DefeasibleRulesNew, DefeasibleRules).

        strictRules(CtrRules, Ids) :-
            findall([RuleName, Preconditions, Effect], (RuleName : Preconditions -> Effect), StrictRulesOld),
            prologStrictRules(StrictRulesNew),
            utils::appendFast(StrictRulesOld, StrictRulesNew, StrictRules),
            transposition::transpose(StrictRules, StrictRules, CtrRules),
            extractId(CtrRules, Ids).

        ordinaryPremises(Premises) :-
            findall([RuleName, Effect], ((RuleName :=> Effect), atom(RuleName)), PremisesOld),
            prologPremises(PremisesNew),
            utils::appendFast(PremisesOld, PremisesNew, Premises).

        axiomPremises(Axioms, Ids) :-
            findall([RuleName, Effect], ((RuleName :-> Effect), atom(RuleName)), AxiomsOld),
            prologAxioms(AxiomsNew),
            utils::appendFast(AxiomsOld, AxiomsNew, Axioms),
            extractId(Axioms, Ids).

        specialRules(SpecialRules) :-
            bpsNew(SpecialRules).



        %! rule(?Id, ?Premises, ?Conclusion)
        %  The rule Id as converted (see convertAllRules/1), concluding Conclusion from Premises.
        rule(Id, PP, [Conclusion]) :-
            rule::rule(Id, Premises, Conclusion, Head, Body),
            context_check(clause(Head, Body)),
            tupleToList(Premises, P),
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


        %! checkStrict(?Id)
        %  The rule or premise Id is strict.
        checkStrict(Id) :- context_check(strict(Id)).

        %! prolog(?Premise, ?Goal)
        %  Premise, as [prolog(Goal)], is a Prolog check.
        prolog([prolog(Term)], Term).
        %! contrary(?Premise, ?Statement)
        %  Premise, as [~(Statement)], is a contrary premise.
        contrary([~(Term)], Term).

        /*
         *   Convert the given tuple to list
         */
        %! tupleToList(+Conjunction, -List)
        %  The items of a conjunction, as a list (built with an accumulator rather than appending the lists of the two
        %  sides, which took a module call at every step)
        tupleToList(T, L) :- tupleToList(T, L, []).
        tupleToList(A, [A|R], R) :- var(A), !.
        tupleToList((A, B), L, R) :- !, tupleToList(A, L, M), tupleToList(B, M, R).
        tupleToList(A, [A|R], R).

        ff([[]], []) :- !.
        ff([[X]], [X]) :- !.
        ff(X, X).

        %! classicRule(?Id, ?Premises, ?Conclusion)
        %  The rule Id concludes Conclusion ([Statement]) from Premises (a list); with metaRules, applicable(Id) is its first premise.
        classicRule(X, [applicable(X)|Y], Z) :- setting(metaRules), !, rule(X, Y, Z).
        classicRule(X, Y, Z) :- rule(X, Y, Z).
        %! classicRule(?Id, ?Conclusion)
        %  The premise Id concludes Conclusion ([Statement]).
        classicRule(X, Y) :- premise(X, Y).

        %! ruleUsing(?Premise, ?Conclusion)
        %  A rule concludes Conclusion (a statement) from Premise, among others: classicRule/3 and member/2 over its
        %  premises, through an index of the premises of the rules built once in the active context.
        ruleUsing(Premise, Conclusion) :-
            premisesIndexed,
            context_check(premiseOf(Premise, Conclusion)).

        % The index: premiseOf/2 facts asserted from the last, as context_check returns the newest clauses first, so
        % that lookups give the rules in the order of classicRule/3
        premisesIndexed :- context_check(premisesIndexed), !.
        premisesIndexed :-
            findall(premiseOf(P, C), (classicRule(_, Premises, [C]), member(P, Premises)), Index),
            assertFromLast(Index),
            context_assert(premisesIndexed).

        assertFromLast([]).
        assertFromLast([Fact|Facts]) :-
            assertFromLast(Facts),
            context_assert(Fact).
        """.trimIndent()
}
