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
    findall(X, context_check(newConc(a, [X])), N),
	buildArgumentsFromRules(N, b, Rules, Rules, n).

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
            context_assert(conc(RuleHead) :- argument([[RuleID], RuleID, RuleHead, RulePrem, [DefRule, DefRule, []]])),
            context_assert(newConc(a, RuleHead)),
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
    findall(X, context_check(newConc(Tn, [X])), N),
    buildArgumentsFromRules(N, Tx, Rules, Rules, n).

invert(a, b).
invert(b, a).

buildArgumentsFromRules(_, _, Rules, [], n) :- !.
buildArgumentsFromRules(_, Tn, Rules, [], y) :- !, clean(Tn, Rules).
buildArgumentsFromRules(N, Tn, Rules, [[Id, Body, Conc]|T], _) :-
%    write([Id, Body, Conc]),nl,
    utils::contains_any(Body, N),
%    write([Id, Body, Conc]),nl,
    findall(true, buildArgumentsFromRule(Tn, [Id, Body, Conc]), R),
    R \== [],
    buildArgumentsFromRules(N, Tn, Rules, T, y), !.
buildArgumentsFromRules(N, Tn, Rules, [_|T], X) :-
    buildArgumentsFromRules(N, Tn, Rules, T, X).

buildArgumentsFromRule(Tn, [RuleID, RuleBody, RuleHead]) :-
	ruleBodyIsSupported(RuleID, RuleHead, RuleBody, RuleBody, [[], [], [], []], Argument),
    context_assert(newConc(Tn, RuleHead)).

checkStrict(Id, []) :- parser::check_strict(Id), !.
checkStrict(Id, [Id]).

% New Argument Info

defRules(Id, Def, Def) :- parser::check_strict(Id), !.
defRules(Id, Def, [Id|Def]).

lastDefRules(Id, LDef, LDef) :- parser::check_strict(Id), !.
lastDefRules(Id, LDef, [Id]) :- Id \== none, \+ parser::check_strict(Id).

argumentInfo(RuleId, LDef, Def, Prem, [FLDef, FDef, FPrem]) :-
    utils::sortDistinct(Prem, FPrem),
    defRules(RuleId, Def, NDef),
    utils::sortDistinct(NDef, FDef),
    lastDefRules(RuleId, LDef, NLDef),
    utils::sortDistinct(NLDef, FLDef).

% Argument Support

ruleBodyIsSupported(RuleID, RuleHead, RuleBody, [], [TAll, TLDef, TDef, TPrem], Argument) :-
    ground(RuleHead),
    \+ context_check(clause(conc(RuleHead), argument([_, RuleID, RuleHead, RuleBody, _]))),
    utils::appendLists(TAll, All),
    \+ utils::contains(RuleID, All),
    utils::appendLists(TLDef, LDef),
    utils::appendLists(TDef, Def),
    utils::appendLists(TPrem, Prem),
    argumentInfo(RuleID, LDef, Def, Prem, Info),
    utils::sort([RuleID|All], Rules),
    Argument = [Rules, RuleID, RuleHead, RuleBody, Info],
    context_assert(conc(RuleHead) :- argument(Argument)),
    context_assert(argument(Argument)),
    utils::hash(argument(Argument), Id),
    context_assert(arg(Id) :- argument(Argument)).
ruleBodyIsSupported(RuleID, RuleHead, RuleBody, [~(_)|Others], Info, Argument) :-
	ruleBodyIsSupported(RuleID, RuleHead, RuleBody, Others, Info, Argument).
ruleBodyIsSupported(RuleID, RuleHead, RuleBody, [prolog(Check)|Others], Info, Argument) :-
	(callable(Check) -> call(Check); Check),
	ruleBodyIsSupported(RuleID, RuleHead, RuleBody, Others, Info, Argument).
ruleBodyIsSupported(RuleId, RuleHead, RuleBody, [Statement|Others], [All, LDef, Def, Prem], Argument) :-
    context_check(clause(conc([Statement]), argument([ArgumentID, RuleID, [Statement], Body, [NLDef, NDef, NPrem]]))),
	ruleBodyIsSupported(RuleId, RuleHead, RuleBody, Others, [[ArgumentID|All], [NLDef|LDef], [NDef|Def], [NPrem|Prem]], Argument),
	context_assert(support([ArgumentID, RuleID, [Statement], Body, [NLDef, NDef, NPrem]], Argument)).

% Attacks

findPossibleAttackers([_, _, Head, _, _], Conf) :-
    conflict(Conf, Head, Guard).
findPossibleAttackers([_, _, Head, _, _], Conf) :-
    conflict(Conf, Head).
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

expanded_conflict(HeadA, HeadB) :-
    conflict(HeadA, HeadB).
expanded_conflict(HeadA, HeadB) :-
    conflict(HeadA, HeadB, Guard),
    (callable(Guard) -> call(Guard); Guard).

%------------------------------------------------------------------------
% Rebutting definition: clash of incompatible conclusions
% we assume a preference relation over arguments determining whether two
% rebutting arguments mutually attack each other or only one of them
% (being preferred) attacks the other
%------------------------------------------------------------------------
rebuts([IDPremisesA, RuleA, RuleHeadA, _, _], [IDPremisesB, RuleB, RuleHeadB, _, Info]) :-
	RuleB \== none,
    Info \== [[], [], []],
	expanded_conflict(RuleHeadA, RuleHeadB).

%------------------------------------------------------------------------
% Contrary Rebutting definition: clash of a conclusion with a failure as premise assumption
%------------------------------------------------------------------------
contraryRebuts([IDPremisesA, RuleA, [RuleHeadA], _, _], [IDPremisesB, RuleB, RuleHeadB, Body, Info]) :-
	RuleA \== none,
	RuleB \== none,
	utils::contains(~(RuleHeadA), Body).

%------------------------------------------------------------------------
% Undermining definition: clash of incompatible premises
%------------------------------------------------------------------------
undermines([IDPremisesA, RuleA, RuleHeadA, _, _], [[IDPremiseB], none, RuleHeadB, _, Info]) :-
	Info \== [[], [], []],
	expanded_conflict(RuleHeadA, RuleHeadB).

%------------------------------------------------------------------------
% Contrary Undermining definition
%------------------------------------------------------------------------
contraryUndermines([IDPremisesA, none, [RuleHeadA], _, _], [IDPremisesB, RuleB, RuleHeadB, Body, Info]) :-
	RuleB \== none,
	utils::contains(~(RuleHeadA), Body).

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

conflict([o(Atom)], [o(-Atom)]) :- \+ check(Atom).
conflict([o(-Atom)], [o(Atom)]).

% conflict([o(Lit)], [-o(Lit)]).
% conflict([-o(Lit)], [o(Lit)]).

conflict([p(Atom)], [o(-Atom)]) :- \+ check(Atom).
conflict([o(-Atom)], [p(Atom)]).

conflict([p(-Atom)], [o(Atom)]).
conflict([o(Atom)], [p(-Atom)]) :- \+ check(Atom).

% BP CONFLICT

% conflict([bp(Atom)], [-bp(Atom)]).
% conflict([-bp(Atom)], [bp(Atom)]).

% SUP CONFLICT

conflict([sup(X, Y)], [sup(Y, X)]).
