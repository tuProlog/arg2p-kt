package it.unibo.tuprolog.argumentation.core.modules.graph.language

import it.unibo.tuprolog.argumentation.core.flags.ArgsFlag
import it.unibo.tuprolog.argumentation.core.flags.AutoTransposition
import it.unibo.tuprolog.argumentation.core.flags.GraphLanguage
import it.unibo.tuprolog.argumentation.core.flags.MetaConflicts
import it.unibo.tuprolog.argumentation.core.flags.MetaRules
import it.unibo.tuprolog.argumentation.core.flags.OpenConclusions
import it.unibo.tuprolog.argumentation.core.flags.PrologStrictCompatibility
import it.unibo.tuprolog.argumentation.core.flags.Temporal
import it.unibo.tuprolog.argumentation.core.modules.PrologModule
import it.unibo.tuprolog.argumentation.core.primitives.graph.language.PrologAxioms
import it.unibo.tuprolog.argumentation.core.primitives.graph.language.PrologDefeasibleRules
import it.unibo.tuprolog.argumentation.core.primitives.graph.language.PrologPremises
import it.unibo.tuprolog.argumentation.core.primitives.graph.language.PrologStrictRules
import it.unibo.tuprolog.argumentation.core.primitives.graph.language.RuleToClause
import it.unibo.tuprolog.argumentation.core.primitives.graph.language.RuleToClauseNatural
import it.unibo.tuprolog.argumentation.core.primitives.graph.language.TheoryBurdens
import it.unibo.tuprolog.argumentation.core.runtime.theory.Notation
import it.unibo.tuprolog.core.operators.OperatorSet
import it.unibo.tuprolog.solve.Signature
import it.unibo.tuprolog.solve.primitive.Primitive

/**
 * The conversion of a theory (module `parser`): convertAllRules/0 converts its rules, premises, preferences and burdens to
 * what the module theory reads, checkLanguages/0 checks it is written in its languages.
 */
object ParserModule : PrologModule("prolog.argumentation.parser", "parser") {
    override val primitives: Map<Signature, Primitive>
        get() =
            mapOf(
                PrologStrictRules.signature to PrologStrictRules,
                PrologAxioms.signature to PrologAxioms,
                TheoryBurdens.signature to TheoryBurdens,
                PrologPremises.signature to PrologPremises,
                PrologDefeasibleRules.signature to PrologDefeasibleRules,
                RuleToClause.signature to RuleToClause,
                RuleToClauseNatural.signature to RuleToClauseNatural,
            )

    // The operators of the rules of a theory, for the theory of the parser and the solvers using it
    override val libraryOperators: OperatorSet
        get() = Notation.rules

    override val theoryOperators: OperatorSet
        get() = Notation.theories

    override val baseFlags: Iterable<ArgsFlag>
        get() = listOf(AutoTransposition, PrologStrictCompatibility, MetaConflicts, MetaRules, GraphLanguage, Temporal, OpenConclusions)

    override val prologRawTheory: String =
        // language=Prolog
        """
        %! convertAllRules
        %  The rules, premises, preferences and burdens of the theory, in the formats the graph builders read (rl/1
        %  clauses, strict/1, sup/2, abstractBp/1), asserted. What the pipeline calls: a term as large as the theory
        %  bound in a resolution (as the Rules of convertAllRules/1) weighs on every binding made after it. Converted
        %  once per solver: the branch is kept as a template, and the solves after the first start from a copy of it.
        convertAllRules :-
            context_template_restore, !.
        convertAllRules :-
            \+ \+ convertAllRules(_),
            context_template_save.

        %! convertAllRules(-Rules)
        %  As convertAllRules/0, with Rules what was asserted.
        convertAllRules(ArgRules) :-
            defeasibleRules(DefeasibleRules),
            ordinaryPremises(Premises),
            axiomPremises(Axioms, AxiomsIds),
            utils::appendLists([DefeasibleRules, Premises, Axioms], Others),
            strictRules(Others, StrictRules, RulesIds),
            theoryBurdens(SpecialRules),
            utils::appendLists([AxiomsIds, RulesIds], StrictIds),
            utils::appendLists([DefeasibleRules, StrictRules, Premises, Axioms], Rules),
            naturalRules(Rules, Clauses, StrictIds, MappedIds),
            findall(sup(X, Y), user::sup(X, Y), Stated),
            supClosure(Stated, Sup),
            languageConflicts(Conflicts),
            utils::appendLists([Clauses, MappedIds, Sup, SpecialRules, Conflicts], ArgRules),
            utils::assertAll(ArgRules),
            boundByQuery(Rules, BoundIds, Constraints),
            indexRules(BoundIds, Constraints), !.

        % indexRules(+BoundIds, +Constraints): those of the rules and premises as written that a query could bind
        % (theory::boundByQuery/1), and whether the theory may have constraints (theory::constrained/0), kept in the context
        % with the rules: computed by a single native visit of them in convertAllRules/1 (see boundByQuery/3)
        indexRules(BoundIds, Constraints) :-
            assertIds(BoundIds),
            (theory::instances -> Kind = yes ; Kind = Constraints),
            context_assert(theoryKind(constrained, Kind)).

        assertIds([]).
        assertIds([Id|Ids]) :- context_assert(boundByQueryRule(Id)), assertIds(Ids).

        % The closure of ground preferences: a round joins the preferences last added (Fresh) with all of them, in the
        % store, by either end: a pair of older ones gave what it gives in an earlier round
        groundSupClosure(Sup, Fresh, Closed) :-
            findall(sup(X, Z), (
                (
                    member(sup(X, Y), Fresh),
                    context_check(supEdge(Y, Z))
                ;
                    member(sup(Y, Z), Fresh),
                    context_check(supEdge(X, Y))
                ),
                X \== Z,
                context_absent(supEdge(X, Z))
            ), New),
            utils::deduplicate(New, Added),
            (Added == [] ->
                Closed = Sup
            ;
                findall(_, (member(sup(X, Z), Added), context_assert(supEdge(X, Z))), _),
                append(Sup, Added, Next),
                groundSupClosure(Next, Added, Closed)
            ).

        %! supClosure(+Stated, -Closed)
        %  The preferences stated, closed transitively: the ordering of rules and premises is a strict partial order
        %  (no sup(X, X) is added, should the stated ones make a cycle). The preferences concluded by arguments
        %  (defeasiblePref) are closed by a rule of the theory instead, as sup(X, Y), sup(Y, Z) -> sup(X, Z).
        supClosure(Sup, Closed) :-
            ground(Sup), !,
            findall(_, (member(sup(X, Y), Sup), context_assert(supEdge(X, Y))), _),
            groundSupClosure(Sup, Sup, Closed),
            context_retract(supEdge(_, _)).
        supClosure(Sup, Closed) :-
            findall(sup(X, Z), (
                member(sup(X, Y), Sup),
                member(sup(Y, Z), Sup),
                X \== Z,
                \+ member(sup(X, Z), Sup)
            ), New),
            utils::deduplicate(New, Fresh),
            (Fresh == [] -> Closed = Sup ; append(Sup, Fresh, Next), supClosure(Next, Closed)).

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
            findall([RuleName, Preconditions, Effect], user::(RuleName : Preconditions => Effect), DefeasibleRulesStated),
            prologDefeasibleRules(DefeasibleRulesOfClauses),
            temporalRules(TemporalRules),
            utils::appendLists([DefeasibleRulesStated, DefeasibleRulesOfClauses, TemporalRules], DefeasibleRules).

        % The axioms of the module temporal, with temporal
        temporalRules(Rules) :- setting(temporal), !, temporal::rules(Rules).
        temporalRules([]).

        % The strict rules with their transpositions, whose names are none of those of the rules and premises of the
        % theory (Others are the other rules and premises)
        strictRules(Others, CtrRules, Ids) :-
            findall([RuleName, Preconditions, Effect], user::(RuleName : Preconditions -> Effect), StrictRulesStated),
            prologStrictRules(StrictRulesOfClauses),
            append(StrictRulesStated, StrictRulesOfClauses, StrictRules),
            ruleIds(StrictRules, StrictIds),
            ruleIds(Others, OtherIds),
            append(StrictIds, OtherIds, Taken),
            transposition::transpose(StrictRules, StrictRules, Taken, CtrRules),
            strictFacts(CtrRules, Ids).

        ordinaryPremises(Premises) :-
            findall([RuleName, Effect], user::(RuleName :=> Effect), PremisesStated),
            prologPremises(PremisesOfClauses),
            languagePremises(PremisesOfLanguages),
            utils::appendLists([PremisesStated, PremisesOfClauses, PremisesOfLanguages], Premises).

        %! languagePremises(-Premises)
        %  The ordinary premises the languages of the theory state (graphLanguage), as [Id, Statement].
        languagePremises(Premises) :-
            findall(P, (setting(graphLanguage, L), L::premises(P)), Lists),
            utils::appendLists(Lists, Premises).

        %! languageConflicts(-Conflicts)
        %  The conflicts the languages of the theory state (graphLanguage), as conflict(Statement, Other).
        languageConflicts(Conflicts) :-
            findall(C, (setting(graphLanguage, L), L::conflicts(C)), Lists),
            utils::appendLists(Lists, Conflicts).

        %! checkLanguages
        %  The theory is written in each of its languages (graphLanguage): each checks it, raising an error if not.
        checkLanguages :-
            findall(L, setting(graphLanguage, L), Languages),
            utils::forEach(L, Languages, L::check).

        axiomPremises(Axioms, Ids) :-
            findall([RuleName, Effect], user::(RuleName :-> Effect), AxiomsStated),
            prologAxioms(AxiomsOfClauses),
            append(AxiomsStated, AxiomsOfClauses, Axioms),
            strictFacts(Axioms, Ids).

        % ruleIds(+Rules, -Ids): the identifiers of Rules, the lists [Id, ...], in their order
        ruleIds([], []).
        ruleIds([[Id|_]|Rules], [Id|Ids]) :- !, ruleIds(Rules, Ids).
        ruleIds([_|Rules], Ids) :- ruleIds(Rules, Ids).

        % strictFacts(+Rules, -Facts): strict(Id) for the identifier of each of Rules
        strictFacts([], []).
        strictFacts([[Id|_]|Rules], [strict(Id)|Facts]) :- strictFacts(Rules, Facts).

        % boundByQuery(+Rules, -Ids, -Constraints): of Rules, as read from the theory ([Id, Premises, Conclusion], the
        % premises as theory::rule/3 reads them, and [Id, Conclusion] for a premise of the theory), Ids are those whose
        % arguments a query could bind otherwise than the whole graph derives them: a variable of the conclusion that no
        % ordinary premise has, or one that a Prolog check (prolog(Goal)) uses before the ordinary premises written before
        % it bind it (X is Expr binds X). For the others the search steered by a query finds the arguments the whole graph
        % has; contraries and constraints ({ C }) bind nothing. Constraints is yes if a premise is a constraint.
        boundByQuery([], [], no).
        boundByQuery([Rule|Rules], Ids, Constraints) :-
            boundByQuery(Rules, OtherIds, OtherConstraints),
            ruleParts(Rule, Id, Premises, Conclusion), !,
            (\+ \+ bindsMore(Premises, Conclusion) -> Ids = [Id|OtherIds] ; Ids = OtherIds),
            (member(Premise, Premises), nonvar(Premise), Premise = {_} -> Constraints = yes ; Constraints = OtherConstraints).
        boundByQuery([_|Rules], Ids, Constraints) :-
            boundByQuery(Rules, Ids, Constraints).

        ruleParts([Id, Premises, Conclusion], Id, List, Conclusion) :-
            theory::tupleToList(Premises, Items),
            theory::premisesListed(Items, List).
        ruleParts([Id, Conclusion], Id, [], Conclusion).

        % The variables are bound as they are met, what is ground is what the premises so far bind (asked within \+ \+)
        bindsMore([], Conclusion) :- \+ ground(Conclusion).
        bindsMore([Premise|Premises], Conclusion) :-
            nonvar(Premise), Premise = prolog(Goal), !,
            (checked(Goal) -> bindsMore(Premises, Conclusion) ; true).
        bindsMore([Premise|Premises], Conclusion) :-
            nonvar(Premise), (Premise = ~(_) ; Premise = {_}), !,
            bindsMore(Premises, Conclusion).
        bindsMore([Premise|Premises], Conclusion) :-
            bindAll(Premise),
            bindsMore(Premises, Conclusion).

        % A check has its variables bound when it runs: a conjunction in its order, X is Expr binding X
        checked(Goal) :- nonvar(Goal), Goal = (A, B), !, checked(A), checked(B).
        checked(Goal) :- nonvar(Goal), Goal = (X is Expr), var(X), !, ground(Expr), bindAll(X).
        checked(Goal) :- ground(Goal).

        bindAll(Term) :- var(Term), !, Term = bound.
        bindAll(Term) :- atomic(Term), !.
        bindAll(Term) :- Term =.. [_|Args], bindEach(Args).
        bindEach([]).
        bindEach([Arg|Args]) :- bindAll(Arg), bindEach(Args).
        """.trimIndent()
}
