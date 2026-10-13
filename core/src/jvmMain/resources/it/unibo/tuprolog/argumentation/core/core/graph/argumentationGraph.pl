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
    \+ filter_meta([R, T, C, B, I]),
    saveArgument(a, A, [[mc|R], T, C, B, I]),
    saveSupport(CArg, [[mc|R], T, C, B, I]),
    cloneSupport([R, T, C, B, I], [[mc|R], T, C, B, I]),
    fail.
buildAdditionalArgument.

cloneSupport(Original, New) :-
    findall(_, (
        context_check(support(S, Original)),
        saveSupport(S, New)
    ), _).

conf_arg([A], Arg) :-
    context_check(clause(conc([conflict(A, _, _)]), argument(Arg))).
conf_arg([A], Arg) :-
    context_check(clause(conc([conflict(A, _)]), argument(Arg))).

filter_meta([Id, _, _, _, _]) :- metaConflicts, member(mc, Id).
filter_meta(_) :- \+ metaConflicts.

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
    context_retract(graphRule(_)),
    storeRules(Rules),
    findall(X, context_check(newConc(a, [X])), N),
	buildArgumentsFromRules(N, b).

% The rules are kept in the context rather than passed around: they are not ground, so carrying their list
% in the goals makes every resolution step copy it. Asserted from the last, as context_check returns the
% newest clauses first.
storeRules([]).
storeRules([Rule|Rules]) :-
    storeRules(Rules),
    context_assert(graphRule(Rule)).

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
            ruleBodyIsSupported(RulePrem, rule(RuleID, RuleHead, RulePrem), [[], [], [], []], _),
            checkStrict(RuleID, DefRule),
            saveArgument(a, RuleHead, [[RuleID], RuleID, RuleHead, RulePrem, [DefRule, DefRule, []]])
        ),
        _
    ).

emptyRule([RuleID, Body, RuleHead]) :-
    parser::classic_rule(RuleID, Body, RuleHead),
    \+ (member(X, Body), X \= ~(_), X \= prolog(_)).

checkStrict(Id, []) :- parser::check_strict(Id), !.
checkStrict(Id, [Id]).


% Argument Construction

% A round fires every rule whose body uses a conclusion derived in the previous round (N), tagging the new
% conclusions with Tn: the rounds go on while they derive something
buildArgumentsFromRules(N, Tn) :-
    findall(_, (
        context_check(graphRule([Id, Body, Conc])),
        utils::contains_any(Body, N),
        buildArgumentsFromRule(Tn, [Id, Body, Conc])
    ), _),
    (context_check(newConc(Tn, _)) -> clean(Tn) ; true).

buildArgumentsFromRule(Tn, [RuleID, RuleBody, RuleHead]) :-
	ruleBodyIsSupported(RuleBody, rule(RuleID, RuleHead, RuleBody), [[], [], [], []], Argument),
	saveArgument(Tn, RuleHead, Argument).

clean(Tn) :-
    invert(Tn, Tx),
    context_retract(newConc(Tx, _)),
    findall(X, context_check(newConc(Tn, [X])), N),
    buildArgumentsFromRules(N, Tx).

invert(a, b).
invert(b, a).


% Argument Info

% A strict rule leaves the defeasible rules as they are, a defeasible one is added to them and becomes the
% only last defeasible rule. The results are bound after the head: repeating a variable in the head would make
% each call compare the lists structurally.
defRules(Id, LDef, Def, NLDef, NDef) :- parser::check_strict(Id), !, NLDef = LDef, NDef = Def.
defRules(Id, _, Def, NLDef, NDef) :- Id \== none, NLDef = [Id], NDef = [Id|Def].

argumentInfo(RuleId, LDef, Def, Prem, Info) :-
    utils::sortDistinct(Prem, FPrem),
    defRules(RuleId, LDef, Def, NLDef, NDef),
    utils::sortDistinct(NDef, FDef),
    utils::sortDistinct(NLDef, FLDef),
    Info = [FLDef, FDef, FPrem].

% Argument Support

% ruleBodyIsSupported(Body, rule(RuleID, RuleHead, RuleBody), [All, LastDefRules, DefRules, DefPremises], Argument):
% Argument is built from RuleID by supporting each statement in Body with an existing argument. Only Body is
% matched in the clause heads, everything else is unpacked in the bodies: matching heads with many arguments
% against every clause is what makes this predicate expensive.
ruleBodyIsSupported([], Rule, Acc, Argument) :-
    Rule = rule(RuleID, RuleHead, RuleBody),
    Acc = [TAll, TLDef, TDef, TPrem],
    utils::appendLists(TAll, All),
    \+ utils::contains(RuleID, All),
    utils::appendLists(TLDef, LDef),
    utils::appendLists(TDef, Def),
    utils::appendLists(TPrem, Prem),
    argumentInfo(RuleID, LDef, Def, Prem, Info),
    utils::sort([RuleID|All], Rules),
    Argument = [Rules, RuleID, RuleHead, RuleBody, Info],
    context_absent(clause(conc(RuleHead), argument(Argument))).
ruleBodyIsSupported([~(_)|Others], Rule, Acc, Argument) :-
	ruleBodyIsSupported(Others, Rule, Acc, Argument).
ruleBodyIsSupported([prolog(Check)|Others], Rule, Acc, Argument) :-
	(callable(Check) -> call(Check); Check),
	ruleBodyIsSupported(Others, Rule, Acc, Argument).
ruleBodyIsSupported([Statement|Others], Rule, Acc, Argument) :-
    Acc = [All, LDef, Def, Prem],
    context_check(clause(conc([Statement]), argument([ArgumentID, RuleID, [Statement], Body, [NLDef, NDef, NPrem]]))),
	ruleBodyIsSupported(Others, Rule, [[ArgumentID|All], [NLDef|LDef], [NDef|Def], [NPrem|Prem]], Argument),
	saveSupport([ArgumentID, RuleID, [Statement], Body, [NLDef, NDef, NPrem]], Argument).


%--------------------------------------------------------------------------
% Build attacks
%--------------------------------------------------------------------------

buildAttacks :-
	findall(_, buildDirectAttack, _).

buildDirectAttack :-
    context_check(argument(A)),
    attack::findPossibleAttackers(A, BB),
    findAttacker(BB, B),
	filter_meta(B),
	A \== B,
	% Attacks are checked on copies, so that A and B are saved exactly as they are stored:
	% binding their variables would change their identifiers (see context_id)
	copy_term(B-A, CB-CA),
    attack::attacks(T, CB, CA),
    saveAttack(T, B, A, A),
    buildTransitiveAttacks(T, B, A),
	fail.
buildDirectAttack.

% Finds the stored arguments whose conclusion unifies with the given one, without instantiating them
findAttacker([Head], B) :- !,
    context_unifiable(conc([Head]), argument(B)).
findAttacker(Conclusion, B) :-
    context_check(clause(conc(Conclusion), argument(B))).

buildTransitiveAttacks(T, B, A) :-
    findall(_, (
        expanded_support(A, C),
        saveAttack(T, B, C, A)
    ),_).

expanded_support(A, B) :-
    context_check(support(A, B)).
expanded_support(A, C) :-
    context_check(support(A, B)),
    expanded_support(B, C).

%--------------------------------------------------------------------------
% Save stuff
%--------------------------------------------------------------------------

saveArgument(T, Conclusion, Argument) :-
    % ground(Conclusion),
    context_absent(clause(conc(Conclusion), argument(Argument))),
    context_assert(newConc(T, Conclusion)),
    context_id(argument(Argument), Id),
    context_assert(argument(Argument)),
    context_assert(conc(Conclusion) :- argument(Argument)),
    context_assert(arg(Id) :- argument(Argument)).

saveAttack(T, A, C, D) :-
    context_absent(attack(T, A, C, D)),
    context_assert(attack(T, A, C, D)),
    context_id(argument(A), IdA),
    context_id(argument(C), IdC),
    context_assert(att(IdA, IdC) :- attack(T, A, C, D)).

saveSupport(A, B) :-
    context_absent(support(A, B)),
    context_assert(support(A, B)).

removeAttack(T, A, B, C) :-
    context_retract(attack(T, A, B, C)),
    context_id(argument(A), IdA),
    context_id(argument(B), IdB),
    context_retract(att(IdA, IdB) :- attack(T, A, B, C)).
