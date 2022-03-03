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
	    parser::classic_rule(RuleID, RuleBody, RuleHead),
	    \+ emptyRule([RuleID, RuleBody, RuleHead])
    ), Rules),
	buildArgumentsFromRules(b, Rules, Rules, n).

buildArgumentsFromPremises :-
    findall(
        _,
        (
            parser::classic_rule(PremiseID, Premise),
            checkStrict(PremiseID, DefPrem),
            ground(Premise),
            context_assert(argument([[PremiseID], none, Premise, [], [[], [], DefPrem]])),
            context_assert(conc(Premise) :- argument([[PremiseID], none, Premise, [], [[], [], DefPrem]])),
            context_assert(newConc(a, Premise)),
            utils::hash(argument([[PremiseID], none, Premise, [], [[], [], DefPrem]]), Id),
            context_assert(arg(Id) :- argument([[PremiseID], none, Premise, [], [[], [], DefPrem]]))
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
            context_assert(argument([[RuleID], RuleID, RuleHead, RulePrem, [DefRule, DefRule, []]])),
            context_assert(conc(Premise) :- argument([[RuleID], RuleID, RuleHead, RulePrem, [DefRule, DefRule, []]])),
            context_assert(newConc(a, Premise)),
            utils::hash(argument([[RuleID], RuleID, RuleHead, RulePrem, [DefRule, DefRule, []]]), Id),
            context_assert(arg(Id) :- argument([[RuleID], RuleID, RuleHead, RulePrem, [DefRule, DefRule, []]]))
        ),
        _
    ).

% Check Rule
emptyRule([RuleID, Body, RuleHead]) :-
    parser::classic_rule(RuleID, Body, RuleHead),
    \+ (member(X, Body), X \= ~(_), X \= prolog(_)),
    checkPrologClauses(Body).

checkPrologClauses([]) :- !.
checkPrologClauses([~(_)|T]) :- checkPrologClauses(T), !.
checkPrologClauses([prolog(Check)|T]) :-
    (callable(Check) -> call(Check); Check),
    checkPrologClauses(T).

% Check \+ member(RuleID, SupportRules) constraint. Is it avoiding cyclical arguments?
% Find best Cut placement

clean(Tn, Rules) :-
    invert(Tn, Tx),
    context_retract(newConc(Tx, _)),
    buildArgumentsFromRules(Tx, Rules, Rules, n).

check_new(Tn, H) :-
    copy_term(H, [_, RuleBody, _]),
    member(X, RuleBody),
    invert(Tn, Tx),
    context_check(newConc(Tx, [X])).

invert(a, b).
invert(b, a).

buildArgumentsFromRules(_, Rules, [], n).
buildArgumentsFromRules(Tn, Rules, [], y) :- clean(Tn, Rules).
buildArgumentsFromRules(Tn, Rules, [H|T], _) :-
    once(check_new(Tn, H)),
    copy_term(H, HH),
    findall(true, buildArgumentsFromRule(Tn, HH), R),
    R \== [],
    buildArgumentsFromRules(Tn, Rules, T, y), !.
buildArgumentsFromRules(Tn, Rules, [_|T], X) :-
    buildArgumentsFromRules(Tn, Rules, T, X).

buildArgumentsFromRule(Tn, [RuleID, RuleBody, RuleHead]) :-
	ruleBodyIsSupported(RuleBody, [], [], SupportRules, ArgSupports),
	\+ member(RuleID, SupportRules),
	ground(RuleHead),
	utils::sort([RuleID|SupportRules], SortedPremises),
	\+ context_check(argument([SortedPremises, RuleID, RuleHead, RuleBody, _])),
	buildArgumentInfo(ArgSupports, RuleID, Info),
    NewArgument = [SortedPremises, RuleID, RuleHead, RuleBody, Info],
    context_assert(conc(RuleHead) :- argument(NewArgument)),
    context_assert(newConc(Tn, RuleHead)),
	context_assert(argument(NewArgument)),
	utils::hash(argument(NewArgument), Id),
	context_assert(arg(Id) :- argument(NewArgument)),
	supports(NewArgument, ArgSupports).

supports(Argument, Supports) :-
    findall(_, (
        member(S, Supports),
        context_assert(support(S, Argument))
    ), _).

checkStrict(Id, [Id]) :- \+ parser::check_strict(Id).
checkStrict(Id, []) :- parser::check_strict(Id).

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
    TopRule \== none, \+ parser::check_strict(TopRule).
lastDefeasibleRules(Supports, TopRule, LastRules) :-
	parser::check_strict(TopRule),
	findall(Def, member([_, _, _, _, [Def, _, _]], Supports), Res),
	utils::appendLists(Res, TempLastRules),
	utils::sortDistinct(TempLastRules, LastRules).

% Argument Support

ruleBodyIsSupported([], ResultPremises, ResultSupports, ResultPremises, ResultSupports).
ruleBodyIsSupported([~(_)|Others], Premises, Supports, ResultPremises, ResultSupports) :-
	ruleBodyIsSupported(Others, Premises, Supports, ResultPremises, ResultSupports).
ruleBodyIsSupported([prolog(Check)|Others], Premises, Supports, ResultPremises, ResultSupports) :-
	(callable(Check) -> call(Check); Check),
	ruleBodyIsSupported(Others, Premises, Supports, ResultPremises, ResultSupports).
ruleBodyIsSupported([Statement|Others], Premises, Supports, ResultPremises, ResultSupports) :-
    context_check(clause(conc([Statement]), argument([ArgumentID, RuleID, [Statement], Body, Info]))),
	append(ArgumentID, Premises, NewPremises),
	ruleBodyIsSupported(Others, NewPremises, [[ArgumentID, RuleID, [Statement], Body, Info]|Supports], ResultPremises, ResultSupports).

% Attacks

findPossibleAttackers([_, _, Head, _, _], Conf) :-
    conflict(Head, Conf).
findPossibleAttackers([_, _, _, Prem, _], [Conf]) :-
    member(~(Conf), Prem).
findPossibleAttackers([_, RuleID, _, _, _], [undercut(RuleID)]).

buildAttacks :-
%    findall(X, context_check(argument(X)), Args),
	buildDirectAttacks,
	buildTransitiveAttacks.

buildDirectAttacks :-
    findall(_, buildDirectAttack, _).


buildDirectAttack :-
    context_check(argument(A)),
    findPossibleAttackers(A, BB),
	context_check(clause(conc(BB), argument(B))),
	A \== B,
    attacks(T, B, A),
	\+ context_check(attack(T, B, A, A)),
	context_assert(attack(T, B, A, A)),
	utils::hash(argument(A), IdA),
	utils::hash(argument(B), IdB),
	context_assert(att(IdB, IdA) :- attack(T, B, A, A)),
	fail.
buildDirectAttack.

buildTransitiveAttacks :-
	context_check(attack(T, A, B, D)),
	context_check(support(B, C)),
	\+ context_check(attack(T, A, C, D)), !,
	context_assert(attack(T, A, C, D)),
    utils::hash(argument(A), IdA),
    utils::hash(argument(C), IdC),
    context_assert(att(IdA, IdC) :- attack(T, B, C, D)),
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
contraryRebuts([IDPremisesA, RuleA, [RuleHeadA], _, _], [IDPremisesB, RuleB, RuleHeadB, Body, Info]) :-
	RuleA \== none,
	RuleB \== none,
	member(~(RuleHeadA), Body).

%------------------------------------------------------------------------
% Undermining definition: clash of incompatible premises
%------------------------------------------------------------------------
undermines([IDPremisesA, RuleA, RuleHeadA, _, _], [[IDPremiseB], none, RuleHeadB, _, Info]) :-
	Info \== [[], [], []],
	conflict(RuleHeadA, RuleHeadB).

%------------------------------------------------------------------------
% Contrary Undermining definition
%------------------------------------------------------------------------
contraryUndermines([IDPremisesA, none, [RuleHeadA], _, _], [IDPremisesB, RuleB, RuleHeadB, Body, Info]) :-
	RuleB \== none,
	member(~(RuleHeadA), Body).

%------------------------------------------------------------------------
% Undercutting definition: attacks on defeasible inference rule
%------------------------------------------------------------------------
undercuts([_, _, [undercut(RuleB)], _, _], [_, RuleB, _, _, [[RuleB], _, _]]).


%========================================================================
% CONFLICT DEFINITION
%========================================================================

check(-Atom).

conflict([Atom], [-Atom]) :- \+ check(Atom).
conflict([-Atom], [Atom]).

conflict([o(Atom)], [o(-Atom)]).
conflict([o(-Atom)], [o(Atom)]).

conflict([o(Lit)], [-o(Lit)]).
conflict([-o(Lit)], [o(Lit)]).

conflict([p(Atom)], [o(-Atom)]).
conflict([o(-Atom)], [p(Atom)]).

conflict([p(-Atom)], [o(Atom)]).
conflict([o(Atom)], [p(-Atom)]).

% BP CONFLICT

% conflict([bp(Atom)], [-bp(Atom)]).
% conflict([-bp(Atom)], [bp(Atom)]).

% SUP CONFLICT

conflict([sup(X, Y)], [sup(Y, X)]).
