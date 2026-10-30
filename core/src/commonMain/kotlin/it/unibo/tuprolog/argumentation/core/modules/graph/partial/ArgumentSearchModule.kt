package it.unibo.tuprolog.argumentation.core.modules.graph.partial

import it.unibo.tuprolog.argumentation.core.modules.PrologModule

/**
 * The arguments for a statement, built on demand (module `argumentSearch`, for query mode): from a premise, or from a
 * rule whose premises are supported in turn by the arguments for them (explored through the module
 * queryExploration, which saves them).
 *
 * An argument is the same term as the graph builder makes (see the module argumentBuilder): its rules and info from
 * those of its sub-arguments (argument::rulesAndInfo/5), its body the premises of its rule as written. Its parts are
 * collected in `found(Body, TopRule, Rules, Info, Supports)`, built in the clause bodies, so that clause heads only
 * match the conclusion or the premises (matching many arguments against every clause head is what makes resolution
 * expensive).
 */
object ArgumentSearchModule : PrologModule("prolog.argumentation.structured.search", "argumentSearch") {
    override val prologRawTheory: String =
        // language=Prolog
        """
        %! findSingleArgument(+Conclusion, -Argument, -Supports)
        %  Argument is an argument for Conclusion, Supports its sub-arguments, saved.
        findSingleArgument(Conclusion, Argument, Supports) :-
            findArgument(Conclusion, found(Body, TopRule, Rules, Info, Supports)),
            argument::new(Rules, TopRule, Conclusion, Body, Info, Argument).

        % The arguments are those the whole graph has (see the module argumentBuilder): Conclusion only steers the search,
        % and an argument concludes what its premises and rule derive, which may not be open (a statement with free
        % variables, but with the graph extension constraints) whatever Conclusion binds.

        % From a premise: one with variables (parser::boundByQuery/1) concludes an open statement, with instances alone
        findArgument(Conclusion, Found) :-
            parser::classicRule(Id, Conclusion),
            (parser::boundByQuery(Id) -> parser::instances ; true),
            premiseInfo(Id, Info),
            Found = found([], none, [Id], Info, []).

        % From a rule: an argument uses a rule at most once (argument::rulesAndInfo/5 fails otherwise), and the rule
        % derives Conclusion from the arguments supporting its premises (see derivable/3)
        findArgument([Claim], Found) :-
            parser::classicRule(Id, Premises, [Claim]),
            findPremises(Premises, premises(Parts, Supports)),
            derivable(Id, Claim, Supports),
            ruleStrictness(Id, Strict),
            argument::rulesAndInfo(Id, Strict, Parts, Rules, Info),
            (parser::constrained -> constraint::extend(Premises, Supports, Body) ; Body = Premises),
            Found = found(Body, Id, Rules, Info, Supports).

        % derivable(+Id, +Claim, +Supports): the rule Id derives Claim from Supports alone. The search steered by the query
        % finds that for most rules; a rule a query could bind otherwise (parser::boundByQuery/1) is taken again as
        % written (a rule whose identifier unifies with Id, concluding a statement with the functor of Claim) and its
        % conclusion derived from the claims of Supports
        derivable(Id, _, _) :- \+ parser::boundByQuery(Id), !.
        derivable(Id, Claim, Supports) :-
            functor(Claim, Name, Arity),
            functor(General, Name, Arity),
            parser::classicRule(Written, Premises, [General]),
            \+ \+ Written = Id,
            derived(rule(Written, Premises, General), Supports, rule(Id, Claim)).

        % derived(+Rule, +Supports, ?Derived): Rule, a copy of the rule as written, derives its conclusion from the claims of
        % Supports alone, its checks run in their order; the conclusion is concludable, and only then the one of Derived
        derived(rule(Id, Premises, Claim), Supports, rule(DerivedId, DerivedClaim)) :-
            fromSupports(Premises, Supports),
            constraint::concludable([Claim]),
            Claim = DerivedClaim,
            Id = DerivedId.

        fromSupports([], []).
        fromSupports([Premise|Premises], Supports) :-
            parser::prolog([Premise], Check), !,
            (callable(Check) -> user::call(Check); user::Check),
            fromSupports(Premises, Supports).
        fromSupports([Premise|Premises], Supports) :-
            \+ ordinaryPremise(Premise), !,
            fromSupports(Premises, Supports).
        fromSupports([Premise|Premises], [Support|Supports]) :-
            argument::claim(Support, Premise),
            fromSupports(Premises, Supports).

        % The premises of a rule are supported one at a time, most selective first (see nextPremise/2): with the
        % bindings of the conclusion and of the premises supported so far, rather than in the order they are written,
        % which could build all the arguments for a premise that the following ones then discard. Each premise has a slot
        % for its result, and the results are collected in the order of the rule: an argument is the same however the
        % premises were supported.
        findPremises(Premises, Found) :-
            premiseSlots(Premises, Slots),
            supportPremises(Slots),
            collectPremises(Slots, Found).

        premiseSlots([], []).
        premiseSlots([Premise|Premises], [slot(Premise, _)|Slots]) :-
            premiseSlots(Premises, Slots).

        supportPremises(Slots) :-
            nextPremise(Slots, Slot), !,
            supportPremise(Slot),
            supportPremises(Slots).
        supportPremises(_).

        % The next premise to support: a Prolog check as soon as the ordinary premises written before it are supported
        % (with the bindings the graph builder gives it, as it may bind variables for the premises after it) or it is
        % ground (when it can only discard); otherwise the premise concluded by the fewest rules; the contrary premises,
        % which are only recorded, last
        nextPremise(Slots, Slot) :-
            readyCheck(Slots, true, Slot), !.
        nextPremise(Slots, Slot) :-
            mostSelective(Slots, none, best(Slot, _)), !.
        nextPremise(Slots, Slot) :-
            member(Slot, Slots),
            Slot = slot(_, Result),
            var(Result), !.

        % readyCheck(+Slots, +Supported, -Slot): the first pending Prolog check that can run, Supported telling whether
        % the ordinary premises before the slots are all supported
        readyCheck([Slot|_], Supported, Slot) :-
            Slot = slot(Premise, Result),
            var(Result),
            parser::prolog([Premise], _),
            (Supported == true ; ground(Premise)), !.
        readyCheck([slot(Premise, Result)|Slots], Supported, Slot) :-
            (var(Result), ordinaryPremise(Premise) -> Next = false ; Next = Supported),
            readyCheck(Slots, Next, Slot).

        % The pending ordinary premise concluded by the fewest rules: each count stops at the best one so far
        mostSelective([], Best, Best).
        mostSelective([Slot|Slots], Best, Selected) :-
            Slot = slot(Premise, Result),
            var(Result),
            ordinaryPremise(Premise), !,
            countLimit(Best, Limit),
            parser::candidates(Premise, Limit, Count),
            selective(Slot, Count, Best, Next),
            mostSelective(Slots, Next, Selected).
        mostSelective([_|Slots], Best, Selected) :-
            mostSelective(Slots, Best, Selected).

        countLimit(none, none).
        countLimit(best(_, Count), Count).

        selective(Slot, Count, none, best(Slot, Count)) :- !.
        selective(Slot, Count, best(_, Min), best(Slot, Count)) :- Count < Min, !.
        selective(_, _, Best, Best).

        ordinaryPremise(Premise) :- parser::argued(Premise).

        supportPremise(slot(Premise, Result)) :-
            parser::prolog([Premise], Term), !,
            (callable(Term) -> user::call(Term); user::Term),
            Result = check.
        supportPremise(slot(Premise, Result)) :-
            parser::contrary([Premise], _), !,
            Result = contrary.
        supportPremise(slot(Premise, Result)) :-
            parser::constraint([Premise], _), !,
            Result = constraint.
        supportPremise(slot(Premise, Result)) :-
            argument::claim(Sub, _),
            queryExploration::buildSubGraph(Premise, Sub),
            Result = argument(Sub).

        % collectPremises(+Slots, -Premises): premises(Parts, Supports), the results of the slots: the rules, last
        % defeasible rules, defeasible rules and defeasible premises of the arguments for the premises (a list of lists
        % each, as argument::rulesAndInfo/5 takes them), and the arguments, in the order of the rule
        collectPremises([], Found) :-
            Found = premises([[], [], [], []], []).
        collectPremises([slot(_, check)|Slots], Found) :- !,
            collectPremises(Slots, Found).
        collectPremises([slot(_, contrary)|Slots], Found) :- !,
            collectPremises(Slots, Found).
        collectPremises([slot(_, constraint)|Slots], Found) :- !,
            collectPremises(Slots, Found).
        collectPremises([slot(_, argument(Sub))|Slots], Found) :-
            collectPremises(Slots, premises([Rules, LastDefRules, DefRules, DefPremises], Supports)),
            argument::rules(Sub, SubRules),
            argument::info(Sub, Info),
            argument::infoParts(Info, SubLastDefRules, SubDefRules, SubDefPremises),
            Found = premises(
                [[SubRules|Rules], [SubLastDefRules|LastDefRules], [SubDefRules|DefRules], [SubDefPremises|DefPremises]],
                [Sub|Supports]
            ).

        % The info of an argument made of the premise Id: none of its premises defeasible if it is strict
        premiseInfo(Id, Info) :- parser::checkStrict(Id), !, argument::infoParts(Info, [], [], []).
        premiseInfo(Id, Info) :- argument::infoParts(Info, [], [], [Id]).

        ruleStrictness(Id, strict) :- parser::checkStrict(Id), !.
        ruleStrictness(_, defeasible).
        """.trimIndent()
}
