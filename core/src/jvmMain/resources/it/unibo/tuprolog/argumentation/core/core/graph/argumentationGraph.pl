% Arguments: [Rules, TopRule, Conclusion, Body, [LastDefRules, DefRules, DefPremises]]
% Support: (Support, Argument)
% Attack: (Type, Attacker, Attacked, On)

buildArgumentationGraph :-
    buildArguments,
    buildAttacks.

buildArguments :-
	buildArgumentsFromPremises,
	buildArgumentsFromEmptyRules,
	findall([RuleID, RuleBody, RuleHead], (
	    context_check(rule([RuleID, RuleBody, RuleHead])),
	    \+ emptyRule([RuleID, RuleBody, RuleHead])
    ), Rules),
	buildArgumentsFromRules(Rules, Rules, n).

buildArgumentsFromPremises :-
    findall(
        _,
        (
            context_check(premise([PremiseID, Premise])),
            checkStrict(PremiseID, DefPrem),
            ground(Premise),
            context_assert(argument([[PremiseID], none, Premise, [], [[], [], DefPrem]]))
        ),
        _
    ).

buildArgumentsFromEmptyRules :-
    findall(
        _,
        (
            emptyRule([RuleID, RulePrem, RuleHead]),
            checkStrict(RuleID, DefRule),
            ground(RuleHead),
            context_assert(argument([[RuleID], RuleID, RuleHead, RulePrem, [DefRule, DefRule, []]]))
        ),
        _
    ).

% Check Rule
emptyRule([RuleID, [], RuleHead]) :-
    context_check(rule([RuleID, [], RuleHead])).
emptyRule([RuleID, [[unless, X]], RuleHead]) :-
    context_check(rule([RuleID, [[unless, X]], RuleHead])).
emptyRule([RuleID, [[prolog(Check)]], RuleHead]) :-
    context_check(rule([RuleID, [[prolog(Check)]], RuleHead])),
    (callable(Check) -> call(Check); Check).


% Check \+ member(RuleID, SupportRules) constraint. Is it avoiding cyclical arguments?
% Find best Cut placement

buildArgumentsFromRules(Rules, [], n).
buildArgumentsFromRules(Rules, [], y) :- buildArgumentsFromRules(Rules, Rules, n).
buildArgumentsFromRules(Rules, [H|T], _) :-
    copy_term(H, HH),
    buildArgumentsFromRule(HH),
    buildArgumentsFromRules(Rules, T, y), !.
buildArgumentsFromRules(Rules, [_|T], X) :-
    buildArgumentsFromRules(Rules, T, X).

buildArgumentsFromRule([RuleID, RuleBody, RuleHead]) :-
	ruleBodyIsSupported(RuleBody, [], [], SupportRules, ArgSupports),
	\+ member(RuleID, SupportRules),
	ground(RuleHead),
	utils::sort([RuleID|SupportRules], SortedPremises),
	\+ context_check(argument([SortedPremises, RuleID, RuleHead, RuleBody, _])),
	buildArgumentInfo(ArgSupports, RuleID, Info),
    NewArgument = [SortedPremises, RuleID, RuleHead, RuleBody, Info],
	context_assert(argument(NewArgument)),
	supports(NewArgument, ArgSupports).

supports(Argument, Supports) :-
    findall(_, (
        member(S, Supports),
        context_assert(support(S, Argument))
    ), _).

checkStrict(Id, [Id]) :- \+ context_check(strict(Id)).
checkStrict(Id, []) :- context_check(strict(Id)).

% Argument Info

buildArgumentInfo(Supports, RuleId, [LastDefRules, DefRules, DefPrem]) :-
    defeasibleRules(RuleId, Supports, DefRules),
    ordinaryPremises(Supports, DefPrem),
    lastDefeasibleRules(Supports, RuleId, LastDefRules).

% Defeasible Premises

ordinaryPremises(Supports, DefPrem) :-
    findall(Def, member([_, _, _, _, [_, _, Def]], Supports), Prem),
    utils::appendLists(Prem, TempPrem),
    utils::sortDistinct(TempPrem, DefPrem).

% Defeasible rules

defeasibleRules(RuleId, Supports, DefRules) :-
	findall(Def, member([_, _, _, _, [_, Def, _]], Supports), UnsortedRules),
	checkStrict(RuleId, DefRule),
	utils::appendLists([DefRule|UnsortedRules], TempRules),
	utils::sortDistinct(TempRules, DefRules).

% Last Defeasible Rules

lastDefeasibleRules(_, TopRule, [TopRule]) :-
    TopRule \== none, \+ context_check(strict(TopRule)).
lastDefeasibleRules(Supports, TopRule, LastRules) :-
	context_check(strict(TopRule)),
	findall(Def, member([_, _, _, _, [Def, _, _]], Supports), Res),
	utils::appendLists(Res, TempLastRules),
	utils::sortDistinct(TempLastRules, LastRules).

% Argument Support

ruleBodyIsSupported([], ResultPremises, ResultSupports, ResultPremises, ResultSupports).
ruleBodyIsSupported([ [unless, _] | Others], Premises, Supports, ResultPremises, ResultSupports) :-
	ruleBodyIsSupported(Others, Premises, Supports, ResultPremises, ResultSupports).
ruleBodyIsSupported([ [prolog(Check)] | Others], Premises, Supports, ResultPremises, ResultSupports) :-
	(callable(Check) -> call(Check); Check),
	ruleBodyIsSupported(Others, Premises, Supports, ResultPremises, ResultSupports).
ruleBodyIsSupported([Statement|Others], Premises, Supports, ResultPremises, ResultSupports) :-
    context_check(argument([ArgumentID, RuleID, Statement, Body, Info])),
	append(ArgumentID, Premises, NewPremises),
	ruleBodyIsSupported(Others, NewPremises, [[ArgumentID, RuleID, Statement, Body, Info]|Supports], ResultPremises, ResultSupports).

% Attacks

findPossibleAttackers([_, _, Head, _, _], [_, _, Conf, _, _]) :-
    conflict(Head, Conf).
findPossibleAttackers([_, _, _, Prem, _], [_, _, Conf, _, _]) :-
    member([unless, Conf], Prem).
findPossibleAttackers([_, RuleID, _, _, _], [_, _, [undercut(RuleID)], _, _]).

buildAttacks :-
    findall(X, context_check(argument(X)), Args),
	buildDirectAttacks(Args),
	buildTransitiveAttacks.

buildDirectAttacks([]).
buildDirectAttacks([H|T]) :-
    findall(_, buildDirectAttack(H), _),
    buildDirectAttacks(T).

% Selezione Attaccanti prima della check su argomento
buildDirectAttack(A) :-
	context_check(argument(B)),
	A \== B,
    attacks(T, B, A),
	\+ context_check(attack(T, B, A, A)),
	context_assert(attack(T, B, A, A)).

buildTransitiveAttacks :-
	context_check(attack(T, A, B, D)),
	context_check(support(B, C)),
	\+ context_check(attack(T, A, C, D)), !,
	context_assert(attack(T, A, C, D)),
    buildTransitiveAttacks.
buildTransitiveAttacks.

% Attack definition
attacks(rebut, A, B) :- rebuts(A, B), !.
attacks(contrary_rebut, A, B) :- contraryRebuts(A, B), !.
attacks(undermine, A, B) :- undermines(A, B), !.
attacks(contrary_undermine, A, B) :- contraryUndermines(A, B), !.
attacks(undercut, A, B) :- undercuts(A, B), !.

%------------------------------------------------------------------------
% Rebutting definition: clash of incompatible conclusions
% we assume a preference relation over arguments determining whether two
% rebutting arguments mutually attack each other or only one of them
% (being preferred) attacks the other
%------------------------------------------------------------------------
rebuts([IDPremisesA, RuleA, RuleHeadA, _, _], [IDPremisesB, RuleB, RuleHeadB, _, Info]) :-
	RuleB \== none,
    Info \== [[], [], []],
	conflict(RuleHeadA, RuleHeadB).

%------------------------------------------------------------------------
% Contrary Rebutting definition: clash of a conclusion with a failure as premise assumption
%------------------------------------------------------------------------
contraryRebuts([IDPremisesA, RuleA, RuleHeadA, _, _], [IDPremisesB, RuleB, RuleHeadB, Body, Info]) :-
	RuleA \== none,
	RuleB \== none,
	member([unless, RuleHeadA], Body).

%------------------------------------------------------------------------
% Undermining definition: clash of incompatible premises
%------------------------------------------------------------------------
undermines([IDPremisesA, RuleA, RuleHeadA, _, _], [[IDPremiseB], none, RuleHeadB, _, Info]) :-
	Info \== [[], [], []],
	conflict(RuleHeadA, RuleHeadB).

%------------------------------------------------------------------------
% Contrary Undermining definition
%------------------------------------------------------------------------
contraryUndermines([IDPremisesA, none, RuleHeadA, _, _], [IDPremisesB, RuleB, RuleHeadB, Body, Info]) :-
	RuleB \== none,
	member([unless, RuleHeadA], Body).

%------------------------------------------------------------------------
% Undercutting definition: attacks on defeasible inference rule
%------------------------------------------------------------------------
undercuts([_, _, [undercut(RuleB)], _, _], [_, RuleB, _, _, [[RuleB], _, _]]).


%========================================================================
% CONFLICT DEFINITION
%========================================================================

conflict( [Atom], [neg, Atom]).
conflict( [neg, Atom], [Atom]).

conflict( [obl, [Atom]],  [obl, [neg, Atom]]).
conflict( [obl, [neg, Atom]],  [obl, [Atom]]).

conflict( [obl, Lit],  [neg, obl, Lit]).
conflict( [neg, obl, Lit],  [obl, Lit]).

conflict( [perm, [Atom]],  [obl, [neg, Atom]]).
conflict( [obl, [neg, Atom]],  [perm, [Atom]]).

conflict( [perm, [neg, Atom]],  [obl, [Atom]]).
conflict( [obl, [Atom]],  [perm, [neg, Atom]]).

% BP CONFLICT

conflict([bp, Atom], [neg, bp, Atom]).
conflict([neg, bp, Atom], [bp, Atom]).

% SUP CONFLICT

conflict([sup(X, Y)],  [sup(Y, X)]).



%========================================================================
% CONFLICT DEFINITION (NEW)
%========================================================================

conflictt([Atom], [-Atom]).
conflictt([-Atom], [Atom]).

conflictt([o(Atom)], [o(-Atom)]).
conflictt([o(-Atom)], [o(Atom)]).

conflictt([o(Lit)], [-o(Lit)]).
conflictt([-o(Lit)], [o(Lit)]).

conflictt([p(Atom)], [o(-Atom)]).
conflictt([o(-Atom)], [p(Atom)]).

conflictt([p(-Atom)], [o(Atom)]).
conflictt([o(Atom)], [p(-Atom)]).

% BP CONFLICT

conflict([bp, Atom], [neg, bp, Atom]).
conflict([neg, bp, Atom], [bp, Atom]).

% SUP CONFLICT

conflict([sup(X, Y)],  [sup(Y, X)]).
