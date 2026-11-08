package it.unibo.tuprolog.argumentation.core.modules.graph.language

import it.unibo.tuprolog.argumentation.core.modules.PrologModule
import it.unibo.tuprolog.argumentation.core.runtime.theory.Notation
import it.unibo.tuprolog.core.operators.OperatorSet

/**
 * What the modules read of a theory once converted (module `theory`, see the module parser): its rules and premises, by
 * rule/3 and premise/2 and through indexes built once in the context, its preferences and burdens, and its kinds (whether
 * its arguments may hold for sets of values, or have constraints).
 */
object TheoryModule : PrologModule("prolog.argumentation.theory", "theory") {
    // The operators of the rules of a theory, as for the parser: for the theory of the module and the solvers using it
    override val libraryOperators: OperatorSet
        get() = Notation.rules

    override val theoryOperators: OperatorSet
        get() = Notation.theories

    override val prologRawTheory: String =
        // language=Prolog
        """
        %! holds(+Check)
        %  Check, a Prolog premise of a rule, holds in the theory of the user.
        holds(Check) :-
            (callable(Check) -> user::call(Check) ; user::Check).

        %! strictness(+Id, -Strictness)
        %  The rule Id is strict or defeasible (see checkStrict/1).
        strictness(Id, strict) :- checkStrict(Id), !.
        strictness(_, defeasible).

        %! preference(?Rule, ?Other)
        %  Rule is preferred to Other: the preferences the theory states (sup/2), closed transitively (see
        %  parser::supClosure/2), or those stated in their place (see statePreferences/1).
        preference(Rule, Other) :-
            context_check(sup(Rule, Other)).

        %! statePreferences(+Preferences)
        %  Preferences, sup(Rule, Other) terms, as the preferences, in place of those stated so far: the extension
        %  defeasiblePref states the preferences concluded by arguments, which alone count then.
        statePreferences(Preferences) :-
            context_retract(sup(_, _)),
            utils::assertAll(Preferences).

        %! burden(?Literals)
        %  The theory states a burden of persuasion on Literals, a list (bp/N facts, abstractBp/1).
        burden(Literals) :-
            context_check(abstractBp(Literals)).

        % The rule Id as written (and converted, see parser::convertAllRules/0), concluding Conclusion from Premises
        ruleWritten(Id, PP, [Conclusion]) :-
            rule::rule(Id, Premises, Conclusion, Head, Body),
            context_check(clause(Head, Body)),
            tupleToList(Premises, P),
            premisesListed(P, PP).
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
        %! argued(+Premise)
        %  Premise, of a rule, is supported by an argument: neither a Prolog check, nor a contrary, nor a constraint.
        argued(Premise) :-
            \+ prolog([Premise], _),
            \+ contrary([Premise], _),
            \+ constraint([Premise], _).
        %! constraint(?Premise, ?Constraint)
        %  Premise, as [{ Constraint }], is a constraint (see the module constraint).
        constraint(['{}'(Constraint)], Constraint).
        %! contrary(?Premise, ?Statement)
        %  Premise, as [~(Statement)], is a contrary premise.
        contrary([~(Term)], Term).

        %! tupleToList(+Conjunction, -List)
        %  The items of a conjunction, as a list (built with an accumulator rather than appending the lists of the two
        %  sides, which took a module call at every step)
        tupleToList(T, L) :- tupleToList(T, L, []).
        tupleToList(A, [A|R], R) :- var(A), !.
        tupleToList((A, B), L, R) :- !, tupleToList(A, L, M), tupleToList(B, M, R).
        tupleToList(A, [A|R], R).

        % The items of the premises of a rule as the list of its premises: none for [], X alone for [X], written as a list
        premisesListed([[]], []) :- !.
        premisesListed([[X]], [X]) :- !.
        premisesListed(X, X).

        %! rule(?Id, ?Premises, ?Conclusion)
        %  The rule Id concludes Conclusion ([Statement]) from Premises (a list); with metaRules, applicable(Id) is its
        %  first premise.
        rule(X, [applicable(X)|Y], Z) :- setting(metaRules), !, ruleWritten(X, Y, Z).
        rule(X, Y, Z) :- ruleWritten(X, Y, Z).

        %! ruleUsing(?Premise, ?Conclusion)
        %  A rule concludes Conclusion (a statement) from Premise, among others: rule/3 and member/2 over its
        %  premises, through the index of the rules built once in the active context.
        ruleUsing(Premise, Conclusion) :-
            rulesIndexed,
            context_check(premiseOf(Premise, Conclusion, _)).

        %! ruleUsingAt(?Premise, ?Place)
        %  The rule at Place (see ruleAt/2) has Premise among its premises.
        ruleUsingAt(Premise, Place) :-
            rulesIndexed,
            context_check(premiseOf(Premise, _, Place)).

        %! ruleAt(?Place, ?Rule)
        %  Rule, [Id, Premises, Conclusion], is the one at Place in the order of rule/3.
        ruleAt(Place, Rule) :-
            rulesPlaced,
            context_check(ruleAt(Place, Rule)).

        % The index: premiseOf/3 facts asserted from the last, as context_check returns the newest clauses first, so
        % that lookups give the rules in the order of rule/3
        rulesIndexed :- context_check(rulesIndexed), !.
        rulesIndexed :-
            findall(premiseOf(P, C, Place), (placedRule(Place, [_, Premises, [C]]), member(P, Premises)), Index),
            assertFromLast(Index),
            context_assert(rulesIndexed).

        %! ruleConclusion(?Id, ?Conclusion)
        %  The rule with identifier Id concludes Conclusion: looked up by its identifier, not among all the rules.
        ruleConclusion(Id, Conclusion) :-
            conclusionsIndexed,
            context_check(ruleIdOf(Id, Conclusion)).

        conclusionsIndexed :- context_check(conclusionsIndexed), !.
        conclusionsIndexed :-
            findall(_, (rule(Id, _, [Conclusion]), context_assert(ruleIdOf(Id, Conclusion))), _),
            context_assert(conclusionsIndexed).

        % The rules themselves, only for who reads them by their places
        rulesPlaced :- context_check(rulesPlaced), !.
        rulesPlaced :-
            findall(_, (placedRule(Place, Rule), context_assert(ruleAt(Place, Rule))), _),
            context_assert(rulesPlaced).

        placedRule(Place, Rule) :-
            findall([Id, Premises, Conclusion], rule(Id, Premises, Conclusion), Rules),
            placed(Rules, 0, Place, Rule).

        placed([Rule|_], Place, Place, Rule).
        placed([_|Rules], At, Place, Rule) :-
            Next is At + 1,
            placed(Rules, Next, Place, Rule).

        %! boundByQuery(?Id)
        %  A query could bind the arguments of the rule or premise Id otherwise than the whole graph derives them: its
        %  conclusion, or a Prolog check, has a variable its ordinary premises written before leave unbound (see
        %  parser::boundByQuery/3).
        boundByQuery(Id) :-
            context_check(boundByQueryRule(Id)).

        assertFromLast([]).
        assertFromLast([Fact|Facts]) :-
            assertFromLast(Facts),
            context_assert(Fact).

        %! instances
        %  The arguments may hold for sets of values, and get their instances.
        instances :- context_check(theoryKind(instances, Kind)), !, Kind == yes.
        instances :-
            (open -> Kind = yes ; Kind = no),
            context_assert(theoryKind(instances, Kind)),
            Kind == yes.

        open :- setting(graphExtension, constraints), !.
        open :- setting(temporal), !.
        open :- setting(openConclusions).

        %! constrained
        %  An argument may have an open conclusion. Otherwise every argument concludes a ground statement.
        constrained :- context_check(theoryKind(constrained, Kind)), !, Kind == yes.
        % (a theory not converted, which parser::indexRules/2 did not state it of)
        constrained :-
            (instances -> Kind = yes ; rule(_, Premises, _), member(P, Premises), constraint([P], _) -> Kind = yes ; Kind = no),
            context_assert(theoryKind(constrained, Kind)),
            Kind == yes.
        """.trimIndent()
}
