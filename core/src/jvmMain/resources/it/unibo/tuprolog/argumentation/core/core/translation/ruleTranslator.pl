/*
 *  Main directive. Find all the rules in the theory in the format (Name : Preconditions => Conclusion)
 *  or (Name : Effects) and then if:
 *  - It is a standard rule, translate it in the standard format (rule([Name, Preconditions, Conclusions]));
 *  - It is a bp rule, translate it to (abstractBp([Effects]))
 *  During the process, clean them from the unallowed symbols (-, o, p).
 */
convertAllRules(ArgRules) :-
    defeasibleRules(DefeasibleRules),
    strictRules(StrictRules, RulesIds),
    ordinaryPremises(Premises),
    axiomPremises(Axioms, AxiomsIds),
    specialRules(SpecialRules),
    utils::appendLists([DefeasibleRules, StrictRules, Premises, Axioms, SpecialRules], L),
    convertAllRules(L, Rules),
    findall(sup(X, Y), sup(X, Y), Sup),
    utils::appendLists([Rules, AxiomsIds, RulesIds, Sup], ArgRules),
    utils::assert_all(ArgRules), !.

defeasibleRules(DefeasibleRules) :-
    findall([RuleName, Preconditions, Effect], (RuleName : Preconditions => Effect), DefeasibleRulesOld),
    prologDefeasibleRules(DefeasibleRulesNew),
    utils::append_fast(DefeasibleRulesOld, DefeasibleRulesNew, DefeasibleRules).

strictRules(CtrRules, Ids) :-
    findall([RuleName, Preconditions, Effect], (RuleName : Preconditions -> Effect), StrictRulesOld),
    prologStrictRules(StrictRulesNew),
    utils::append_fast(StrictRulesOld, StrictRulesNew, StrictRules),
    transpose(StrictRules, StrictRules, CtrRules),
    extract_id(CtrRules, Ids).

ordinaryPremises(Premises) :-
    findall([RuleName, Effect], ((RuleName :=> Effect), atom(RuleName)), PremisesOld),
    prologPremises(PremisesNew),
    utils::append_fast(PremisesOld, PremisesNew, Premises).

axiomPremises(Axioms, Ids) :-
    findall([RuleName, Effect], ((RuleName :-> Effect), atom(RuleName)), AxiomsOld),
    prologAxioms(AxiomsNew),
    utils::append_fast(AxiomsOld, AxiomsNew, Axioms),
    extract_id(Axioms, Ids).

specialRules(SpecialRules) :-
    bpsNew(SpecialRules).
%    findall([bps, X], search('bp', 10, X), SpecialRules).

%=======================================================================================================================
% TRANSPOSITION
%=======================================================================================================================

transpose(_, CtrRules, CtrRules) :- \+ autoTransposition, !.
transpose([], CtrRules, CtrRules).
transpose([H|T], TempCtrRules, CtrRules) :-
    transpose(T, TempCtrRules, CR),
    findall(TrH, transposition(H, TrH), Tran),
    mergeCtrRules(CR, Tran, CtrRules).

mergeCtrRules(All, ToMerge, X) :-
    utils::append_fast(All, ToMerge, XT),
    deduplicate(XT, X).

deduplicate([], []).
deduplicate([[RN,A,B]|T], X) :- deduplicate(T, TT), (member([RN,_,_], TT) -> X = TT; X = [[RN,A,B]|TT]). 

transposition([Id, Prec, Effect], [Id, Prec, Effect]).
transposition([Id, Prec, Effect], [NewId, NewPrec ,XNegated]) :-
    compound(Prec),
    tuple_to_list(Prec, LPrec),!,
    transposition_sequential(LPrec, LPrec, Effect, Id, [], NewPrec, XNegated, NewId).
transposition([Id, Prec, Effect], [NewId, EffectNegated ,XNegated]) :-
    \+ compound(Prec),
    Prec \== [],
    prologEscape(Prec),
    negate(Prec, XNegated),
    negate(Effect, EffectNegated),
    atom_concat(Id, '_i', NewId).

transposition_sequential(LPrec, [H|T], Effect, Id, Skipped, NewPrec, XNegated, NewId) :-
    transposition_sequential(LPrec, T, Effect, Id, [H|Skipped], NewPrec, XNegated, NewId).
transposition_sequential(LPrec, [X|T], Effect, Id, Skipped, NewPrec, XNegated, NewId) :-
    prologEscape(X),
    utils::append_fast(Skipped, T, CleanedPrec),
    negate(X, XNegated),
    negate(Effect, EffectNegated),
    list_to_tuple([EffectNegated|CleanedPrec], NewPrec),
    newIdentifier(Skipped, Id, NewId).

newIdentifier(List, OldId, NewId) :-
    modifier(List, Mod),
    atom_concat(OldId, Mod, NewId).

modifier([], '_i').
modifier([_|Tail], Index):-
    modifier(Tail, Index1),
    atom_concat(Index1, 'i', Index).

negate(X, Arg) :-
    functor(X, '-', _) -> (
        arg(1, X, Arg));
    functor(X, 'p', _) -> (
        arg(1, X, PX),
        (functor(PX, '-', _) -> (
            arg(1, PX, PNX),
            Arg = o(PNX)
        );
        Arg = o(-(PX))));
    Arg = -(X).

prologEscape(X) :- \+ functor(X, 'prolog', _), \+ functor(X, '~', _).

%=======================================================================================================================

/*
 *   Standard rules (RuleName : Preconditions => Effects)
 *   Special rules (RuleName : Effects)
 */

convertAllRules([], []).
convertAllRules([[H,P,E]|T], [R|Rules]) :- convertRule(H, P, E, R), convertAllRules(T, Rules).
convertAllRules([[H,E]|T], [R|Rules]) :- convertRule(H, E, R), convertAllRules(T, Rules).

/*
 *   Convert the given rule to the standard format
 *   Example:
 *   r2: followedGuidelines(X), doctor(X) => -liable(X)
 *   rule([r2,[[followedGuidelines(X_e4149)],[doctor(X_e4149)]],[neg,liable(X_e4149)]]).
 */
convertRule(RuleName, Preconditions, Effects, rule(List)) :-
    tuple_to_list(Preconditions, Lprecond),
    tuple_to_list(Effects, Leffects),
    check_modifiers_in_list(preconditions, Lprecond, LprecondChecked),
    check_modifiers_in_list(effects, Leffects, LeffectsChecked),
    flatten_first_level(LeffectsChecked, LeffectsCheckedFlattened),
    List = [RuleName, LprecondChecked, LeffectsCheckedFlattened], !.

/*
 *   Convert the given special rule
 *   Example:
 *   bp(-liable(X)).
 *   abastractBp([[neg, liable(X_e4149)]]).
 */
convertRule(bps, Effects, abstractBp(Checked)) :-
    functor(Effects, 'bp', _),
    Effects =.. L,
    removehead(L, LC),
    check_modifiers_in_list(effects, LC, Checked), !.

convertRule(Name, Effects, premise([Name, LeffectsCheckedFlattened])) :-
    tuple_to_list(Effects, Leffects),
    check_modifiers_in_list(effects, Leffects, LeffectsChecked),
    flatten_first_level(LeffectsChecked, LeffectsCheckedFlattened).

%=======================================================================================================================

/*
 *   Find negations(-), obligations(o), permissions(p) on a list of preconditions/effects and
 *   raplace them with the assigned literal (neg, obl, perm)
 */
check_modifiers_in_list(MODE, [], []) :- !.
check_modifiers_in_list(MODE, [H|T], L) :- H == [], L = [], !.
check_modifiers_in_list(MODE, [H|T], L) :- H \== [],
                            check_modifiers_once(H, LH),
                            % check_admissibility(MODE, H, LH),
                            check_modifiers_in_list(MODE, T, LT),
                            utils::append_fast([LH], LT, L).

check_modifiers_once(H, List) :- once(check_modifiers(H, List)).

check_modifiers([], []).
check_modifiers(H, List) :-
    functor(H, '-', _) -> (
        arg(1, H, Arg),
        check_modifiers_once(Arg, Lobl),
        utils::append_fast([neg], Lobl, Lf),
        List = Lf);
    functor(H, 'o', _) -> (
        arg(1, H, Arg),
        check_modifiers_once(Arg, Lobl),
        List = [obl|[Lobl]]);
    functor(H, 'p', _) -> (
        arg(1, H, Arg),
        check_modifiers_once(Arg, Lper),
        List = [perm|[Lper]]);
    functor(H, '~', _) -> (
        arg(1, H, Arg),
        check_modifiers_once(Arg, Lper),
        List = [unless|[Lper]]);
    functor(H, 'bp', _) -> (
        H =.. [_|Arg],
        check_modifiers_in_list(effects, Arg, Lper),
        List = [bp|[Lper]]);
    List = [H].

/*
 *   Convert the given tuple to list
 */
tuple_to_list(A,[A]) :- nonvar(A), A \= (_ , _), !.
tuple_to_list((A,B),L) :-
    tuple_to_list(A, La),
    tuple_to_list(B, Lb),
    utils::append_fast(La, Lb,L).

list_to_tuple([H], (H)) :- !.
list_to_tuple([H|T], (H,TT)) :- list_to_tuple(T,TT).

/*
 *   Replace all the occurences of a given element with the given argument
 */
replace(_, _, [],[]) :- !.
replace(O, R, [O|T], [R|T2]) :- replace(O, R, T, T2), !.
replace(O, R, [H|T], [H|T2]) :- H \= O, replace(O, R, T, T2).

flatten_first_level([X], X) :- !.
flatten_first_level.

removehead([_|Tail], Tail).

in(A, A) :- nonvar(A), A \= (_ , _), !.
in(A, (A, _)) :- !.
in(A, (_ , Cs)) :- in(A, Cs).

% Admissibility check

check_admissibility(preconditions, H, LH) :-
    \+ defeasible_admissible(LH),
    throw(['Premise  ', H, '  is not a well formed member of the argumentation language.']).
check_admissibility(effects, H, LH) :-
    \+ admissible(LH),
    throw(['Conclusion  ', H, '  is not a well formed member of the argumentation language.']).
check_admissibility(_, _, _).

defeasible_admissible([unless, Term]) :- admissible(Term).
defeasible_admissible(Term) :- admissible(Term).

admissible([neg, bp, Term]) :- admissible_terms_complete(Term).
admissible([bp, Term]) :- admissible_terms_complete(Term).
admissible([neg, Term]) :- admissible_term(Term).
admissible([obl, [Term]]) :- admissible_term(Term).
admissible([obl, [neg, Term]]) :- admissible_term(Term).
admissible([neg, obl, [Term]]) :- admissible_term(Term).
admissible([neg, obl, [neg, Term]]) :- admissible_term(Term).
admissible([perm, [Term]]) :- admissible_term(Term).
admissible([perm, [neg, Term]]) :- admissible_term(Term).
admissible([Term]) :- admissible_term(Term).

admissible_term(Term) :-
    atomic(Term),
    Term \== neg,
    Term \== obl,
    Term \== perm,
    Term \== unless,
    Term \== bp.
admissible_term(Term) :- var(Term).
admissible_term(Term) :-
    compound(Term),
    \+ functor(Term, 'o', _),
    \+ functor(Term, 'p', _),
    \+ functor(Term, '-', _),
    \+ functor(Term, '~', _),
    \+ functor(Term, 'bp', _),
    Term =.. [_|Args],
    admissible_terms(Args).

admissible_terms([]).
admissible_terms([H|T]) :-
    admissible_term(H),
    admissible_terms(T).

admissible_terms_complete([]).
admissible_terms_complete([H|T]) :-
    admissible(H),
    admissible_terms_complete(T).
