package it.unibo.tuprolog.argumentation.core.modules.query

import it.unibo.tuprolog.argumentation.core.modules.PrologModule

/**
 * The arguments for a statement, built on demand (module `argumentSearch`, for query mode): from a premise, or from a
 * rule whose premises are supported in turn by the arguments for them (explored through the module
 * queryExploration, which saves them).
 *
 * While an argument is searched, what its sub-arguments use is collected in a term
 * `used(Rules, LastDefeasibleRules, DefeasibleRules, DefeasiblePremises)`, and its parts in
 * `found(Body, TopRule, Used, Supports)`: built in the clause bodies, so that clause heads only match the conclusion
 * or the premises (matching many arguments against every clause head is what makes resolution expensive).
 */
object ArgumentSearchModule : PrologModule("prolog.argumentation.structured.search", "argumentSearch") {
    override val prologRawTheory: String =
        """
        %! findSingleArgument(+Conclusion, -Argument, -Supports)
        %  Argument is an argument for Conclusion, Supports its sub-arguments, saved.
        findSingleArgument(Conclusion, Argument, Supports) :-
            findArgument(Conclusion, [], found(Body, TopRule, used(Rules, LastDefRules, DefRules, DefPremises), Supports)),
            utils::deduplicate(DefRules, CDefRules),
            utils::deduplicate(DefPremises, CDefPremises),
            utils::deduplicate(Body, CBody),
            argument::infoParts(Info, LastDefRules, CDefRules, CDefPremises),
            argument::new(Rules, TopRule, Conclusion, CBody, Info, Argument).

        % From a premise
        findArgument(Conclusion, _, Found) :-
            parser::classic_rule(Id, Conclusion),
            premiseRules(Id, Used),
            Found = found([], none, Used, []).

        % From a rule: an argument uses a rule at most once (as in the whole graph, see argument::rulesAndInfo/5)
        findArgument(Conclusion, Ids, Found) :-
            parser::classic_rule(Id, Premises, Conclusion),
            findPremises(Premises, [Id|Ids], premises(Conclusions, SubUsed, Supports)),
            SubUsed = used(SubRules, _, _, _),
            \+ member(Id, SubRules),
            ruleRules(Id, SubUsed, Used),
            Found = found(Conclusions, Id, Used, Supports).

        % The premises of a rule are supported one at a time, most selective first (see nextPremise/2): with the
        % bindings of the conclusion and of the premises supported so far, rather than in the order they are written,
        % which could build all the arguments for a premise that the following ones then discard. Each premise has a slot
        % for its result, and the results are collected in the order of the rule: an argument is the same however the
        % premises were supported.
        findPremises(Premises, _, Found) :-
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

        ordinaryPremise(Premise) :-
            \+ parser::prolog([Premise], _),
            \+ parser::contrary([Premise], _).

        supportPremise(slot(Premise, Result)) :-
            parser::prolog([Premise], Term), !,
            (callable(Term) -> call(Term); Term),
            Result = check.
        supportPremise(slot(Premise, Result)) :-
            parser::contrary([Premise], _), !,
            Result = contrary.
        supportPremise(slot(Premise, Result)) :-
            argument::claim(Sub, _),
            queryExploration::buildSubGraph(Premise, Sub),
            Result = argument(Sub).

        % collectPremises(+Slots, -Premises): premises(Conclusions, Used, Supports), the results of the slots, in
        % their order: the conclusions of the premises (contraries included), what their arguments use, and the
        % arguments
        collectPremises([], Found) :-
            Found = premises([], used([], [], [], []), []).
        collectPremises([slot(_, check)|Slots], Found) :- !,
            collectPremises(Slots, Found).
        collectPremises([slot(Premise, contrary)|Slots], Found) :- !,
            collectPremises(Slots, premises(Conclusions, Used, Supports)),
            Found = premises([Premise|Conclusions], Used, Supports).
        collectPremises([slot(_, argument(Sub))|Slots], Found) :-
            collectPremises(Slots, premises(Conclusions, Used, Supports)),
            argument::claim(Sub, Claim),
            argument::rules(Sub, Rules),
            argument::info(Sub, Info),
            argument::infoParts(Info, LastDefRules, DefRules, DefPremises),
            mergeUsed(used(Rules, LastDefRules, DefRules, DefPremises), Used, Merged),
            Found = premises([Claim|Conclusions], Merged, [Sub|Supports]).

        % What an argument made of the premise Id uses
        premiseRules(Id, Used) :- parser::check_strict(Id), !, Used = used([Id], [], [], []).
        premiseRules(Id, used([Id], [], [], [Id])).

        % What an argument made of the rule Id uses, with its sub-arguments using SubUsed (a single check_strict
        % call: the second clause is only reached for defeasible rules)
        ruleRules(Id, used(Rules, LastDefRules, DefRules, DefPremises), Used) :-
            parser::check_strict(Id), !,
            Used = used([Id|Rules], LastDefRules, DefRules, DefPremises).
        ruleRules(Id, used(Rules, _, DefRules, DefPremises), used([Id|Rules], [Id], [Id|DefRules], DefPremises)).

        mergeUsed(used(R1, L1, D1, P1), used(R2, L2, D2, P2), used(R, L, D, P)) :-
            utils::appendLists([R1, R2], R),
            utils::appendLists([L1, L2], L),
            utils::appendLists([D1, D2], D),
            utils::appendLists([P1, P2], P).
        """.trimIndent()
}
