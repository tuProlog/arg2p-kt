% Arguments: [Rules, TopRule, Conclusion, Body, [LastDefRules, DefRules, DefPremises]]
% Support: (Support, Argument)
% Attack: (Type, Attacker, Attacked, On)

buildArgumentationGraph :-
    buildArguments,
    findall(_, buildAdditionalArgument, _),
    buildAttacks.

%--------------------------------------------------------------------------
% Build Conflict Arguments
%--------------------------------------------------------------------------
buildAdditionalArgument :-
    metaConflicts,
    conf_arg(A, CArg),
    context_check(clause(conc(A), argument([R, T, C, B, I]))),
    saveArgument(a, A, [[mc|R], T, C, B, I]),
    context_assert(support(CArg, [[mc|R], T, C, B, I])),
    cloneSupport([R, T, C, B, I], [[mc|R], T, C, B, I]),
    fail.
buildAdditionalArgument.

cloneSupport(Original, New) :-
    findall(_, (
        context_check(support(S, Original)),
        context_assert(support(S, New))
    ), _).

conf_arg([A], Arg) :-
    context_check(clause(conc([conflict(A, _, _)]), argument(Arg))).
conf_arg([A], Arg) :-
    context_check(clause(conc([conflict(A, _)]), argument(Arg))).

%--------------------------------------------------------------------------
% Build Standard Arguments
%--------------------------------------------------------------------------
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

emptyRule([RuleID, Body, RuleHead]) :-
    parser::classic_rule(RuleID, Body, RuleHead),
    \+ (member(X, Body), X \= ~(_), X \= prolog(_)),
    ruleBodyIsSupported(RuleID, RuleHead, Body, Body, [[], [], [], []], _).

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


%--------------------------------------------------------------------------
% Build attacks
%--------------------------------------------------------------------------

buildAttacks :-
	findall(_, buildDirectAttack, _),
	buildTransitiveAttacks.

buildDirectAttack :-
    context_check(argument(A)),
    attack::findPossibleAttackers(A, BB),
	context_check(clause(conc(BB), argument(B))),
	filter_meta(B),
	A \== B,
    attack::attacks(T, B, A),
    saveAttack(T, B, A, A),
	fail.
buildDirectAttack.

buildTransitiveAttacks :-
	context_check(attack(T, A, B, D)),
	context_check(support(B, C)),
	saveAttack(T, A, C, D),!,
    buildTransitiveAttacks.
buildTransitiveAttacks.

filter_meta([Id, _, _, _, _]) :- metaConflicts, member(mc, Id).
filter_meta(_) :- \+ metaConflicts.

%--------------------------------------------------------------------------
% Save stuff
%--------------------------------------------------------------------------
saveArgument(T, Conclusion, Argument) :-
    ground(Conclusion),
    \+ context_check(clause(conc(Conclusion), argument(Argument))),
    context_assert(newConc(T, Conclusion)),
    utils::hash(argument(Argument), Id),
    context_assert(argument(Argument)),
    context_assert(conc(Conclusion) :- argument(Argument)),
    context_assert(arg(Id) :- argument(Argument)).


saveAttack(T, A, C, D) :-
    \+ context_check(attack(T, A, C, D)),
    context_assert(attack(T, A, C, D)),
    utils::hash(argument(A), IdA),
    utils::hash(argument(C), IdC),
    context_assert(att(IdA, IdC) :- attack(T, A, C, D)).