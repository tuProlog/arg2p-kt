%! buildArguments
%  The arguments made of a premise, those of the rules with no premise to support, and then, round after round,
%  those of the rules using a conclusion of the previous round.
buildArguments :-
    mode(Mode),
    buildArgumentsFromPremises(Mode),
    buildArgumentsFromEmptyRules(Mode),
    findall(X, store::newConclusion(a, [X]), N),
    buildArgumentsFromRules(N, b, Mode).

% mode(-Mode): base, for a theory whose arguments all conclude ground statements with no constraint to keep, or
% constrained (constraints::constrained/0): asked once, the arguments of a theory in the base mode built as if
% constraints were not there
mode(constrained) :- constraints::constrained, !.
mode(base).

buildArgumentsFromPremises(Mode) :-
    findall(_, (
        parser::premise(PremiseID, Premise),
        concludable(Mode, Premise),
        parser::strictness(PremiseID, Strict),
        store::premiseInfo(PremiseID, Strict, Info),
        store::newArgument([PremiseID], none, Premise, [], Info, Argument),
        store::saveArgument(a, Premise, Argument)
    ), _).

buildArgumentsFromEmptyRules(Mode) :-
    findall(_, (
        emptyRule([RuleID, RulePrem, RuleHead]),
        supported(Mode, RulePrem, RuleID, RuleHead, Argument),
        store::saveArgument(a, RuleHead, Argument)
    ), _).

% A rule with no premise to support with an argument: only contraries, Prolog checks and constraints (see
% rule::premiseKind/2)
emptyRule([RuleID, Body, RuleHead]) :-
    parser::rule(RuleID, Body, RuleHead),
    \+ (member(X, Body), parser::premiseKind(X, statement)).

% A round fires every rule whose body uses a conclusion derived in the previous round (N), reached through the
% index of the rules (module parser) rather than going through all the rules, in their order, tagging the new conclusions with
% Tn: the rounds go on while they derive something
buildArgumentsFromRules(N, Tn, Mode) :-
    findall(Place, (member(Conclusion, N), parser::ruleUsingAt(Conclusion, Place)), Reached),
    utils::deduplicate(Reached, Places),
    findall(_, (
        member(Place, Places),
        parser::ruleAt(Place, [Id, Body, Conc]),
        buildArgumentsFromRule(Tn, [Id, Body, Conc], Mode)
    ), _),
    (store::newConclusion(Tn, _) -> nextRound(Tn, Mode) ; true).

buildArgumentsFromRule(Tn, [RuleID, RuleBody, RuleHead], Mode) :-
    supported(Mode, RuleBody, RuleID, RuleHead, Argument),
    store::saveArgument(Tn, RuleHead, Argument).

nextRound(Tn, Mode) :-
    otherTag(Tn, Tx),
    store::forgetNewConclusions(Tx),
    findall(X, store::newConclusion(Tn, [X]), N),
    buildArgumentsFromRules(N, Tx, Mode).

otherTag(a, b).
otherTag(b, a).


% supported(+Mode, +Body, +RuleID, +RuleHead, -Argument): Argument is built from the rule RuleID supporting each
% statement of its premises Body with a saved argument, as the mode of the theory asks
supported(Mode, Body, RuleID, RuleHead, Argument) :-
    bodySupported(Body, Mode, rule(RuleID, RuleHead, Body), [[], [], [], []], [], Argument).

% An argument may conclude Conclusion: ground, or as the module constraints says in a theory with constraints
concludable(_, Conclusion) :- ground(Conclusion), !.
concludable(constrained, Conclusion) :- constraints::concludable(Conclusion).

% bodySupported(+Premises, +Mode, +Rule, +Parts, +Supports, -Argument): Argument is built from Rule, rule(RuleID,
% RuleHead, RuleBody), by supporting each statement of Premises with a saved argument, collecting the rules and premises
% of the supports (Parts, see store::accumulated/3) and the supports; the Prolog checks hold, the contraries and the
% constraints are kept in its body. In a theory with constraints (Mode constrained), the constraints of the rule and of
% the supports, with their bindings, must hold together, those still open kept in the body of Argument, and its
% conclusion may be open; a support used for fewer values than it has is saved as bound (its instance, which the module
% constraints adds when the graph is changed).
bodySupported([], Mode, rule(RuleID, RuleHead, RuleBody), Parts, Supports, Argument) :-
    concludable(Mode, RuleHead),
    parser::strictness(RuleID, Strict),
    store::rulesAndInfo(RuleID, Strict, Parts, Rules, Info),
    argumentBody(Mode, RuleBody, Supports, Body),
    store::newArgument(Rules, RuleID, RuleHead, Body, Info, Argument),
    \+ store::argumentFor(RuleHead, Argument).
bodySupported([Premise|Premises], Mode, Rule, Parts, Supports, Argument) :-
    parser::premiseKind(Premise, Kind),
    premiseSupported(Kind, Premise, Premises, Mode, Rule, Parts, Supports, Argument).

premiseSupported(statement, Statement, Premises, Mode, Rule, Parts, Supports, Argument) :- !,
    store::argumentFor([Statement], Support),
    store::accumulated(Support, Parts, Accumulated),
    bodySupported(Premises, Mode, Rule, Accumulated, [Support|Supports], Argument),
    store::saveSupport(Support, Argument).
premiseSupported(check(Check), _, Premises, Mode, Rule, Parts, Supports, Argument) :- !,
    parser::holds(Check),
    bodySupported(Premises, Mode, Rule, Parts, Supports, Argument).
premiseSupported(_, _, Premises, Mode, Rule, Parts, Supports, Argument) :-
    bodySupported(Premises, Mode, Rule, Parts, Supports, Argument).

% The body of the argument: the premises of its rule, with the constraints of its supports still open in a theory with
% constraints
argumentBody(base, Body, _, Body).
argumentBody(constrained, RuleBody, Supports, Body) :- constraints::extend(RuleBody, Supports, Body).
