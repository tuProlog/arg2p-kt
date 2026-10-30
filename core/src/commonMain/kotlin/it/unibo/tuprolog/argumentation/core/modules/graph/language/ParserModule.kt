package it.unibo.tuprolog.argumentation.core.modules.graph.language

import it.unibo.tuprolog.argumentation.core.flags.ArgsFlag
import it.unibo.tuprolog.argumentation.core.flags.AutoTransposition
import it.unibo.tuprolog.argumentation.core.flags.GraphLanguage
import it.unibo.tuprolog.argumentation.core.flags.MetaConflicts
import it.unibo.tuprolog.argumentation.core.flags.MetaRules
import it.unibo.tuprolog.argumentation.core.flags.PrologStrictCompatibility
import it.unibo.tuprolog.argumentation.core.modules.PrologModule
import it.unibo.tuprolog.argumentation.core.primitives.graph.language.BpsNew
import it.unibo.tuprolog.argumentation.core.primitives.graph.language.ExtractId
import it.unibo.tuprolog.argumentation.core.primitives.graph.language.NeedsIndex
import it.unibo.tuprolog.argumentation.core.primitives.graph.language.OccursFreeUnify
import it.unibo.tuprolog.argumentation.core.primitives.graph.language.PrologAxioms
import it.unibo.tuprolog.argumentation.core.primitives.graph.language.PrologDefeasibleRules
import it.unibo.tuprolog.argumentation.core.primitives.graph.language.PrologPremises
import it.unibo.tuprolog.argumentation.core.primitives.graph.language.PrologStrictRules
import it.unibo.tuprolog.argumentation.core.primitives.graph.language.RuleToClause
import it.unibo.tuprolog.argumentation.core.primitives.graph.language.RuleToClauseNatural
import it.unibo.tuprolog.argumentation.core.runtime.settings.Profiles
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
                NeedsIndex::descriptionPair.get(),
                OccursFreeUnify::descriptionPair.get(),
                RuleToClause::descriptionPair.get(),
                RuleToClauseNatural::descriptionPair.get(),
            )

    // The operators of the rules of a theory, for the theory of the parser and the solvers using it
    override val libraryOperators: OperatorSet
        get() = operators()

    override val theoryOperators: OperatorSet
        get() = super.theoryOperators.plus(operators())

    override val baseFlags: Iterable<ArgsFlag<*, *>>
        get() = listOf(AutoTransposition, PrologStrictCompatibility, MetaConflicts, MetaRules, GraphLanguage)

    fun operators() =
        OperatorSet(
            Operator("=>", Specifier.XFX, 1199),
            Operator(":=>", Specifier.XFX, 1199),
            Operator(":->", Specifier.XFX, 1199),
            Operator(":", Specifier.XFX, 1001),
//            Operator("->", Specifier.XFX, 1050)
        ) + Profiles.operators()

    override val prologRawTheory: String =
        // language=Prolog
        """
        %! convertAllRules(-Rules)
        %  The rules, premises, preferences and burdens of the theory, in the formats the graph builders read (rl/1 clauses, strict/1, sup/2, abstractBp/1), asserted; Rules are what was asserted.
        convertAllRules(ArgRules) :-
            defeasibleRules(DefeasibleRules),
            ordinaryPremises(Premises),
            axiomPremises(Axioms, AxiomsIds),
            utils::appendLists([DefeasibleRules, Premises, Axioms], Others),
            strictRules(Others, StrictRules, RulesIds),
            specialRules(SpecialRules),
            utils::appendLists([AxiomsIds, RulesIds], StrictIds),
            utils::appendLists([DefeasibleRules, StrictRules, Premises, Axioms], Rules),
            naturalRules(Rules, Clauses, StrictIds, MappedIds),
            findall(sup(X, Y), user::sup(X, Y), Stated),
            supClosure(Stated, Sup),
            languageConflicts(Conflicts),
            utils::appendLists([Clauses, MappedIds, Sup, SpecialRules, Conflicts], ArgRules),
            utils::assertAll(ArgRules), !.

        %! supClosure(+Stated, -Closed)
        %  The preferences stated, closed transitively: the ordering of rules and premises is a strict partial order
        %  (no sup(X, X) is added, should the stated ones make a cycle). The preferences concluded by arguments
        %  (defeasiblePref) are closed by a rule of the theory instead, as sup(X, Y), sup(Y, Z) -> sup(X, Z).
        supClosure(Sup, Closed) :-
            findall(sup(X, Z), (
                member(sup(X, Y), Sup),
                member(sup(Y, Z), Sup),
                X \== Z,
                \+ member(sup(X, Z), Sup)
            ), New),
            utils::deduplicate(New, Fresh),
            (Fresh == [] -> Closed = Sup ; utils::appendFast(Sup, Fresh, Next), supClosure(Next, Closed)).

        naturalRules(Rules, Clauses, StrictIds, MappedIds) :-
            user::naturalTerms,
            ruleToClauseNatural(Rules, Clauses),
            mapIds(Rules, Clauses, StrictIds, MappedIds).
        naturalRules(Rules, Clauses, StrictIds, StrictIds) :-
            \+ user::naturalTerms,
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
            findall([RuleName, Preconditions, Effect], user::(RuleName : Preconditions => Effect), DefeasibleRulesOld),
            prologDefeasibleRules(DefeasibleRulesNew),
            utils::appendFast(DefeasibleRulesOld, DefeasibleRulesNew, DefeasibleRules).

        % The strict rules with their transpositions, whose names are none of those of the rules and premises of the
        % theory (Others are the other rules and premises)
        strictRules(Others, CtrRules, Ids) :-
            findall([RuleName, Preconditions, Effect], user::(RuleName : Preconditions -> Effect), StrictRulesOld),
            prologStrictRules(StrictRulesNew),
            utils::appendFast(StrictRulesOld, StrictRulesNew, StrictRules),
            findall(Id, (member([Id|_], StrictRules) ; member([Id|_], Others)), Taken),
            transposition::transpose(StrictRules, StrictRules, Taken, CtrRules),
            extractId(CtrRules, Ids).

        ordinaryPremises(Premises) :-
            findall([RuleName, Effect], (user::(RuleName :=> Effect), atom(RuleName)), PremisesOld),
            prologPremises(PremisesNew),
            languagePremises(PremisesLanguages),
            utils::appendLists([PremisesOld, PremisesNew, PremisesLanguages], Premises).

        %! languagePremises(-Premises)
        %  The ordinary premises the languages of the theory state (graphLanguage), as [Id, Statement].
        languagePremises(Premises) :-
            findall(P, (setting(graphLanguage, L), L:::premises(P)), Lists),
            utils::appendLists(Lists, Premises).

        %! languageConflicts(-Conflicts)
        %  The conflicts the languages of the theory state (graphLanguage), as conflict(Statement, Other).
        languageConflicts(Conflicts) :-
            findall(C, (setting(graphLanguage, L), L:::conflicts(C)), Lists),
            utils::appendLists(Lists, Conflicts).

        %! checkLanguages
        %  The theory is written in each of its languages (graphLanguage): each checks it, raising an error if not.
        checkLanguages :-
            findall(L, setting(graphLanguage, L), Languages),
            utils::forEach(L, Languages, L:::check).

        axiomPremises(Axioms, Ids) :-
            findall([RuleName, Effect], (user::(RuleName :-> Effect), atom(RuleName)), AxiomsOld),
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
        %! argued(+Premise)
        %  Premise, of a rule, is supported by an argument: neither a Prolog check nor a contrary.
        argued(Premise) :-
            \+ prolog([Premise], _),
            \+ contrary([Premise], _).
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

        %! complete(+Statement)
        %  The arguments for Statement found with the bindings it has include those for every statement it generalises:
        %  no rule concluding it needs a variable of its conclusion that Statement leaves unbound (see checksIndexed).
        complete(Statement) :-
            \+ neededBy(Statement, _).

        %! neededBy(+Statement, -Vars)
        %  A rule concluding Statement needs Vars bound, which Statement leaves unbound (in part, at least).
        neededBy(Statement, Vars) :-
            checksIndexed,
            functor(Statement, Name, Arity),
            functor(Conclusion, Name, Arity),
            context_check(checkNeeds(Conclusion, Vars)),
            occursFreeUnify(Conclusion, Statement),
            \+ ground(Vars).

        %! needing(?Id)
        %  The rule Id needs variables of its conclusion bound by the statement asked for (see complete/1): the arguments
        %  it makes for a statement may be missing from those found for a statement generalising it.
        needing(Id) :-
            checksIndexed,
            context_check(needingRule(Id)).

        % The index: checkNeeds(Conclusion, Vars) and needingRule(Id) for each rule Id needing the variables Vars of its
        % conclusion bound by the statement asked for (see needsIndex/2), built once in the active context
        checksIndexed :- context_check(checksIndexed), !.
        checksIndexed :-
            findall(rule(Id, C, Premises), classicRule(Id, Premises, [C]), Rules),
            needsIndex(Rules, Index),
            assertFromLast(Index),
            context_assert(checksIndexed).
        """.trimIndent()
}
