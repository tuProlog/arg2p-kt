%! findSingleArgument(+Conclusion, -Argument, -Supports)
%  Argument is an argument for Conclusion, Supports its sub-arguments, saved.
findSingleArgument(Conclusion, Argument, Supports) :-
    findArgument(Conclusion, found(Body, TopRule, Rules, Info, Supports)),
    store::newArgument(Rules, TopRule, Conclusion, Body, Info, Argument).

% The arguments are those the whole graph has (see the module argumentBuilder): Conclusion only steers the search,
% and an argument concludes what its premises and rule derive, which may not be open (a statement with free
% variables, but with the graph extension constraints) whatever Conclusion binds.

% From a premise: one with variables (boundByQuery/1) concludes an open statement, with instances alone
findArgument(Conclusion, Found) :-
    parser::premise(Id, Conclusion),
    (boundByQuery(Id) -> constraints::instances ; true),
    parser::strictness(Id, Strict),
    store::premiseInfo(Id, Strict, Info),
    Found = found([], none, [Id], Info, []).

% From a rule: an argument uses a rule at most once (store::rulesAndInfo/5 fails otherwise), and the rule
% derives Conclusion from the arguments supporting its premises (see derivable/3)
findArgument([Claim], Found) :-
    parser::rule(Id, Premises, [Claim]),
    findPremises(Premises, premises(Parts, Supports)),
    derivable(Id, Claim, Supports),
    parser::strictness(Id, Strict),
    store::rulesAndInfo(Id, Strict, Parts, Rules, Info),
    (constraints::constrained -> constraints::extend(Premises, Supports, Body) ; Body = Premises),
    Found = found(Body, Id, Rules, Info, Supports).

% derivable(+Id, +Claim, +Supports): the rule Id derives Claim from Supports alone. The search steered by the query
% finds that for most rules; a rule a query could bind otherwise (boundByQuery/1) is taken again as
% written (a rule whose identifier unifies with Id, concluding a statement with the functor of Claim) and its
% conclusion derived from the claims of Supports
derivable(Id, _, _) :- \+ boundByQuery(Id), !.
derivable(Id, Claim, Supports) :-
    functor(Claim, Name, Arity),
    functor(General, Name, Arity),
    parser::rule(Written, Premises, [General]),
    \+ \+ Written = Id,
    derived(rule(Written, Premises, General), Supports, rule(Id, Claim)).

% derived(+Rule, +Supports, ?Derived): Rule, a copy of the rule as written, derives its conclusion from the claims of
% Supports alone, its checks run in their order; the conclusion is concludable, and only then the one of Derived
derived(rule(Id, Premises, Claim), Supports, rule(DerivedId, DerivedClaim)) :-
    fromSupports(Premises, Supports),
    constraints::concludable([Claim]),
    Claim = DerivedClaim,
    Id = DerivedId.

fromSupports([], []).
fromSupports([Premise|Premises], Supports) :-
    parser::premiseKind(Premise, Kind),
    fromSupportsAs(Kind, Premise, Premises, Supports).

fromSupportsAs(check(Check), _, Premises, Supports) :- !,
    parser::holds(Check),
    fromSupports(Premises, Supports).
fromSupportsAs(statement, Premise, Premises, [Support|Supports]) :- !,
    store::claim(Support, Premise),
    fromSupports(Premises, Supports).
fromSupportsAs(_, _, Premises, Supports) :-
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
    parser::premiseKind(Premise, check(_)),
    (Supported == true ; ground(Premise)), !.
readyCheck([slot(Premise, Result)|Slots], Supported, Slot) :-
    (var(Result), parser::premiseKind(Premise, statement) -> Next = false ; Next = Supported),
    readyCheck(Slots, Next, Slot).

% The pending ordinary premise concluded by the fewest rules: each count stops at the best one so far
mostSelective([], Best, Best).
mostSelective([Slot|Slots], Best, Selected) :-
    Slot = slot(Premise, Result),
    var(Result),
    parser::premiseKind(Premise, statement), !,
    countLimit(Best, Limit),
    parser::countConcluding(Premise, Limit, Count),
    selective(Slot, Count, Best, Next),
    mostSelective(Slots, Next, Selected).
mostSelective([_|Slots], Best, Selected) :-
    mostSelective(Slots, Best, Selected).

countLimit(none, none).
countLimit(best(_, Count), Count).

selective(Slot, Count, none, best(Slot, Count)) :- !.
selective(Slot, Count, best(_, Min), best(Slot, Count)) :- Count < Min, !.
selective(_, _, Best, Best).

supportPremise(slot(Premise, Result)) :-
    parser::premiseKind(Premise, Kind),
    supportedAs(Kind, Premise, Result).

% The result of the slot of a premise of the kind Kind (see rule::premiseKind/2): a check holding, a contrary or a
% constraint recorded, or each argument for a statement
supportedAs(check(Goal), _, Result) :- !,
    parser::holds(Goal),
    Result = check.
supportedAs(contrary(_), _, Result) :- !,
    Result = contrary.
supportedAs(constraint(_), _, Result) :- !,
    Result = constraint.
supportedAs(statement, Premise, Result) :-
    store::claim(Sub, _),
    queryExploration::buildSubGraph(Premise, Sub),
    Result = argument(Sub).

% collectPremises(+Slots, -Premises): premises(Parts, Supports), the results of the slots: the rules, last
% defeasible rules, defeasible rules and defeasible premises of the arguments for the premises (a list of lists
% each, as store::rulesAndInfo/5 takes them), and the arguments, in the order of the rule
collectPremises([], Found) :-
    Found = premises([[], [], [], []], []).
collectPremises([slot(_, check)|Slots], Found) :- !,
    collectPremises(Slots, Found).
collectPremises([slot(_, contrary)|Slots], Found) :- !,
    collectPremises(Slots, Found).
collectPremises([slot(_, constraint)|Slots], Found) :- !,
    collectPremises(Slots, Found).
collectPremises([slot(_, argument(Sub))|Slots], Found) :-
    collectPremises(Slots, premises(Parts, Supports)),
    store::accumulated(Sub, Parts, Accumulated),
    Found = premises(Accumulated, [Sub|Supports]).

