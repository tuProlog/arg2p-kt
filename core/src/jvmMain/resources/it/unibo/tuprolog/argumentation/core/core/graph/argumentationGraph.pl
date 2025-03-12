% Arguments: [Rules, TopRule, Conclusion, Body, [LastDefRules, DefRules, DefPremises]]
% Support: (Support, Argument)
% Attack: (Type, Attacker, Attacked, On)

buildArgumentationGraph :-
    buildArguments,
    findall(_, buildAdditionalArguments, _),
    buildAttacks.


buildAdditionalArguments :-
    metaConflicts,
    conf_arg(A, CArg),
    context_check(clause(conc(A), argument([R, T, C, B, I]))),
    saveArgument(a, A, [[mc|R], T, C, B, I]),
    context_assert(support(CArg, [[mc|R], T, C, B, I])),
    cloneSupport([R, T, C, B, I], [[mc|R], T, C, B, I]),
    fail.
buildAdditionalArguments.

cloneSupport(Original, New) :-
    findall(_, (
        context_check(support(S, Original)),
        context_assert(support(S, New))
    ), _).

conf_arg([A], Arg) :-
    context_check(clause(conc([conflict(A, _, _)]), argument(Arg))).
conf_arg([A], Arg) :-
    context_check(clause(conc([conflict(A, _)]), argument(Arg))).


buildArguments :-
	buildArgumentsFromPremises,
	buildArgumentsFromEmptyRules,
	findall([RuleID, RuleBody, RuleHead], (
	    parser::classic_rule(RuleID, RuleBody, RuleHead),
	    \+ emptyRule([RuleID, RuleBody, RuleHead])
    ), Rules),
    findall(X, context_check(newConc(a, [X])), N),
	buildArgumentsFromRules(N, b, Rules, Rules, n).


saveArgument(T, Conclusion, Argument) :-
    ground(Conclusion),
    \+ context_check(clause(conc(Conclusion), argument(Argument))),
    context_assert(newConc(T, Conclusion)),
    utils::hash(argument(Argument), Id),
    context_assert(argument(Argument)),
    context_assert(conc(Conclusion) :- argument(Argument)),
    context_assert(arg(Id) :- argument(Argument)).


buildArgumentsFromPremises :-
    findall(
        _,
        (
            parser::classic_rule(PremiseID, Premise),
            checkStrict(PremiseID, DefPrem),
            saveArgument(a, Premise, [[PremiseID], none, Premise, [], [[], [], DefPrem]])
        ),
        _
    ).

buildArgumentsFromEmptyRules :-
    findall(
        _,
        (
            emptyRule([RuleID, RulePrem, RuleHead]),
            checkStrict(RuleID, DefRule),
            saveArgument(a, RuleHead, [[RuleID], RuleID, RuleHead, RulePrem, [DefRule, DefRule, []]])
        ),
        _
    ).

% Check Rule
emptyRule([RuleID, Body, RuleHead]) :-
    parser::classic_rule(RuleID, Body, RuleHead),
    \+ (member(X, Body), X \= ~(_), X \= prolog(_)),
    ruleBodyIsSupported(RuleID, RuleHead, Body, Body, [[], [], [], []], _).
%    checkPrologClauses(Body).

checkPrologClauses([]) :- !.
checkPrologClauses([~(_)|T]) :- checkPrologClauses(T), !.
checkPrologClauses([prolog(Check)|T]) :-
    (callable(Check) -> call(Check); Check),
    checkPrologClauses(T).


checkStrict(Id, []) :- parser::check_strict(Id), !.
checkStrict(Id, [Id]).


% Argument Construction

buildArgumentsFromRules(_, _, Rules, [], n) :- !.
buildArgumentsFromRules(_, Tn, Rules, [], y) :- !, clean(Tn, Rules).
buildArgumentsFromRules(N, Tn, Rules, [[Id, Body, Conc]|T], _) :-
    utils::contains_any(Body, N),
    findall(true, buildArgumentsFromRule(Tn, [Id, Body, Conc]), R),
    R \== [],
    buildArgumentsFromRules(N, Tn, Rules, T, y), !.
buildArgumentsFromRules(N, Tn, Rules, [_|T], X) :-
    buildArgumentsFromRules(N, Tn, Rules, T, X).

buildArgumentsFromRule(Tn, [RuleID, RuleBody, RuleHead]) :-
	ruleBodyIsSupported(RuleID, RuleHead, RuleBody, RuleBody, [[], [], [], []], Argument),
	saveArgument(Tn, RuleHead, Argument).

clean(Tn, Rules) :-
    invert(Tn, Tx),
    context_retract(newConc(Tx, _)),
    findall(X, context_check(newConc(Tn, [X])), N),
    buildArgumentsFromRules(N, Tx, Rules, Rules, n).

invert(a, b).
invert(b, a).


% Argument Info

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
    utils::appendLists(TAll, All),
    \+ utils::contains(RuleID, All),
    utils::appendLists(TLDef, LDef),
    utils::appendLists(TDef, Def),
    utils::appendLists(TPrem, Prem),
    argumentInfo(RuleID, LDef, Def, Prem, Info),
    utils::sort([RuleID|All], Rules),
    Argument = [Rules, RuleID, RuleHead, RuleBody, Info],
    \+ context_check(clause(conc(RuleHead), argument(Argument))).
ruleBodyIsSupported(RuleID, RuleHead, RuleBody, [~(_)|Others], Info, Argument) :-
	ruleBodyIsSupported(RuleID, RuleHead, RuleBody, Others, Info, Argument).
ruleBodyIsSupported(RuleID, RuleHead, RuleBody, [prolog(Check)|Others], Info, Argument) :-
	(callable(Check) -> call(Check); Check),
	ruleBodyIsSupported(RuleID, RuleHead, RuleBody, Others, Info, Argument).
ruleBodyIsSupported(RuleId, RuleHead, RuleBody, [Statement|Others], [All, LDef, Def, Prem], Argument) :-
    context_check(clause(conc([Statement]), argument([ArgumentID, RuleID, [Statement], Body, [NLDef, NDef, NPrem]]))),
	ruleBodyIsSupported(RuleId, RuleHead, RuleBody, Others, [[ArgumentID|All], [NLDef|LDef], [NDef|Def], [NPrem|Prem]], Argument),
	context_assert(support([ArgumentID, RuleID, [Statement], Body, [NLDef, NDef, NPrem]], Argument)).






buildAttacks :-
	buildDirectAttacks,
	buildTransitiveAttacks.


% Attacks

findPossibleAttackers([_, _, Head, _, _], Conf) :-
    parser::classic_conflict(Conf, Head, Guard).
findPossibleAttackers([_, _, Head, _, _], Conf) :-
    parser::classic_conflict(Conf, Head).
findPossibleAttackers([_, _, _, Prem, _], [Conf]) :-
    member(~(Conf), Prem).
findPossibleAttackers([_, RuleID, _, _, _], [undercut(RuleID)]).
findPossibleAttackers([_, RuleID, _, _, _], Conf) :-
    parser::classic_conflict(Conf, [RuleID], Guard).
findPossibleAttackers([_, RuleID, _, _, _], Conf) :-
    parser::classic_conflict(Conf, [RuleID]).


filter_meta([Id, _, _, _, _]) :- metaConflicts, member(mc, Id).
filter_meta(_) :- \+ metaConflicts.


buildDirectAttacks :-
    findall(_, buildDirectAttack, _).

buildDirectAttack :-
    context_check(argument(A)),
    findPossibleAttackers(A, BB),
    %write(BB),nl,
	context_check(clause(conc(BB), argument(B))),
	%write(B),nl,
	filter_meta(B),
	%write(after_filter),nl,
	A \== B,
    attacks(T, B, A),
    %write(T),nl,
	\+ context_check(attack(T, B, A, A)),
	context_assert(attack(T, B, A, A)),
	utils::hash(argument(A), IdA),
	utils::hash(argument(B), IdB),
	context_assert(att(IdB, IdA) :- attack(T, B, A, A)),
	%write(attack(T, B, A, A)),nl,
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


%------------------------------------------------------------------------
% Attack definition
%------------------------------------------------------------------------

attacks(rebut, A, B) :- rebuts(A, B), !.
attacks(contrary_rebut, A, B) :- contraryRebuts(A, B), !.
attacks(undermine, A, B) :- undermines(A, B), !.
attacks(contrary_undermine, A, B) :- contraryUndermines(A, B), !.
attacks(undercut, A, B) :- undercuts(A, B), !.

expanded_conflict(HeadA, HeadB) :-
    parser::classic_conflict(HeadA, HeadB).
expanded_conflict(HeadA, HeadB) :-
    parser::classic_conflict(HeadA, HeadB, Guard),
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

%------------------------------------------------------------------------
% Raw Undercutting definition: attacks on defeasible inference rule
%------------------------------------------------------------------------
undercuts([_, _, NegRuleB, _, _], [_, RuleB, _, _, [[RuleB], _, _]]) :-
    expanded_conflict(NegRuleB, [RuleB]).

%========================================================================
% CONFLICT DEFINITION
%========================================================================

% check(-Atom).

% conflict([Atom], [-Atom]) :- \+ check(Atom).
conflict([Atom], [-Atom], (Atom \= -_)).
conflict([-Atom], [Atom]).

% conflict([o(Atom)], [o(-Atom)]) :- \+ check(Atom).
conflict([o(Atom)], [o(-Atom)], (Atom \= -_)).
conflict([o(-Atom)], [o(Atom)]).

% conflict([o(Lit)], [-o(Lit)]).
% conflict([-o(Lit)], [o(Lit)]).

% conflict([p(Atom)], [o(-Atom)]) :- \+ check(Atom).
conflict([p(Atom)], [o(-Atom)], (Atom \= -_)).
conflict([o(-Atom)], [p(Atom)]).

conflict([p(-Atom)], [o(Atom)]).
% conflict([o(Atom)], [p(-Atom)]) :- \+ check(Atom).
conflict([o(Atom)], [p(-Atom)], (Atom \= -_)).

% BP CONFLICT

% conflict([bp(Atom)], [-bp(Atom)]).
% conflict([-bp(Atom)], [bp(Atom)]).

% SUP CONFLICT

conflict([sup(X, Y)], [sup(Y, X)]).
