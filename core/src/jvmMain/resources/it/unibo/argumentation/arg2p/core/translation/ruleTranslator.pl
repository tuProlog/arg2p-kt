% ----------------------------------------------------------------
% ruleTranslator.pl
% PIKA-lab
% Year: 2019
% -------------------------------------------------------------------------------

% rule_name : o(-what_is_mandatory_to_avoid), o(what_is_mandatory), p(what_is_permitted), p(-what_is_permitted), what_is_true, -what_is_true => effect.
% are converted into lists expressing rules
% rule([v, [ [obl, [neg, enter]], [enter] ], [violation] ]).
% rule([rPerm, [ [emer] ], [perm, [enter]] ]).

:- op(1199, xfx, '-->').
:- op(1199, xfx, '~~>').

:- op(1199, xfx, ':>').
:- op(1199, xfx, '=>').
:- op(1001, xfx, ':').

in(A, A) :- nonvar(A), A \= (_ , _).
in(A, (A, _)).
in(A, (_ , Cs)) :- in(A, Cs).

/*
 *  Main directive. Find all the rules in the theory in the format (Name : Preconditions => Conclusion)
 *  or (Name : Effects) and then if:
 *  - It is a standard rule, translate it in the standard format (rule([Name, Preconditions, Conclusions]));
 *  - It is a bp rule, translate it to (abstractBp([Effects]))
 *  During the process, clean them from the unallowed symbols (-, o, p).
 */
convertAllRules :-
    retractall(rule(_)),
    retractall(premise(_)),
    retractall(abstractBp(_)),
    retractall(reifiedBp(_)),
    retractall(strict(_)),
    defeasibleRules(DefeasibleRules),
    strictRules(StrictRules),
    ordinaryPremises(Premises),
    axiomPremises(Axioms),
    specialRules(SpecialRules),
    appendLists([DefeasibleRules, StrictRules, Premises, Axioms, SpecialRules], L),
    convertAllRules(L), !.

defeasibleRules(DefeasibleRules) :-
    findall([RuleName, Preconditions, Effect], (RuleName : Preconditions => Effect), DefeasibleRules).

strictRules(CtrRules) :-
    findall([RuleName, Preconditions, Effect], (RuleName : Preconditions :> Effect), StrictRules),
    transpose(StrictRules, StrictRules, CtrRules),
    findall(_, (member([RN, _, _], CtrRules), assert(strict(RN))), _).

ordinaryPremises(Premises) :-
    findall([RuleName, Effect], (RuleName ~~> Effect), Premises).

axiomPremises(Axioms) :-
    findall([RuleName, Effect], (RuleName --> Effect), Axioms),
    findall(_, (member([RN, _], Axioms), assert(strict(RN))), _).

specialRules(SpecialRules) :-
    findall([_, X], search('bp', 10, X), SpecialRules).

%=======================================================================================================================
% TRANSPOSITION
%=======================================================================================================================

transpose([], CtrRules, CtrRules).
transpose([H|T], TempCtrRules, CtrRules) :-
    transpose(T, TempCtrRules, CR),
    findall(TrH, transposition(H, TrH), Tran),
    mergeCtrRules(CR, Tran, CtrRules).

mergeCtrRules(All, ToMerge, X) :-
    append(All, ToMerge, XT),
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
    append(Skipped, T, CleanedPrec),
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


prologEscape(X) :- \+ functor(X, 'prolog', _).

%=======================================================================================================================

/*
 *   Standard rules (RuleName : Preconditions => Effects)
 *   Special rules (RuleName : Effects)
 */

convertAllRules([]).
convertAllRules([[H,P,E]|T]) :- convertRule(H, P, E), convertAllRules(T).
convertAllRules([[H,E]|T]) :- convertRule(H, E), convertAllRules(T).

/*
 *   Convert the given rule to the standard format
 *   Example:
 *   r2: followedGuidelines(X), doctor(X) => -liable(X)
 *   rule([r2,[[followedGuidelines(X_e4149)],[doctor(X_e4149)]],[neg,liable(X_e4149)]]).
 */
convertRule(RuleName, Preconditions, Effects) :-
    tuple_to_list(Preconditions, Lprecond),
    tuple_to_list(Effects, Leffects),
    check_modifiers_in_list(preconditions, Lprecond, LprecondChecked),
    check_modifiers_in_list(effects, Leffects, LeffectsChecked),
    flatten_first_level(LeffectsChecked, LeffectsCheckedFlattened),
    List = [RuleName, LprecondChecked, LeffectsCheckedFlattened],
    assert(rule(List)).

/*
 *   Convert the given special rule
 *   Example:
 *   bp(-liable(X)).
 *   abastractBp([[neg, liable(X_e4149)]]).
 */
convertRule(_, Effects) :-
    functor(Effects, 'bp', _),
    Effects =.. L,
    removehead(L, LC),
    check_modifiers_in_list(effects, LC, Checked),
    assert(abstractBp(Checked)).

convertRule(Name, Effects) :-
    \+ functor(Effects, 'bp', _),
    tuple_to_list(Effects, Leffects),
    check_modifiers_in_list(effects, Leffects, LeffectsChecked),
    flatten_first_level(LeffectsChecked, LeffectsCheckedFlattened),
    assert(premise([Name, LeffectsCheckedFlattened])).

%=======================================================================================================================

/*
 *   Find negations(-), obligations(o), permissions(p) on a list of preconditions/effects and
 *   raplace them with the assigned literal (neg, obl, perm)
 */
check_modifiers_in_list(MODE, [], []).
check_modifiers_in_list(MODE, [H|T], L) :- H == [], L = [].
check_modifiers_in_list(MODE, [H|T], L) :- H \== [],
                            check_modifiers(H, LH),
                            check_admissibility(MODE, H, LH),
                            check_modifiers_in_list(MODE, T, LT),
                            append([LH], LT, L).

check_admissibility(preconditions, H, LH) :-
    \+ defeasible_admissible(LH),
    throw(['Premise  ', H, '  is not a well formed member of the argumentation language.']).
check_admissibility(effects, H, LH) :-
    \+ admissible(LH),
    throw(['Conclusion  ', H, '  is not a well formed member of the argumentation language.']).
check_admissibility(_, _, _).

check_modifiers([], []).
check_modifiers(H, List) :-
    functor(H, '-', _) -> (
        arg(1, H, Arg),
        check_modifiers(Arg, Lobl),
        append([neg], Lobl, Lf),
        List = Lf);
    functor(H, 'o', _) -> (
        arg(1, H, Arg),
        check_modifiers(Arg, Lobl),
        List = [obl|[Lobl]]);
    functor(H, 'p', _) -> (
        arg(1, H, Arg),
        check_modifiers(Arg, Lper),
        List = [perm|[Lper]]);
    functor(H, '~', _) -> (
        arg(1, H, Arg),
        check_modifiers(Arg, Lper),
        List = [unless|[Lper]]);
    List = [H].

/*
 *   Convert the given tuple to list
 */
tuple_to_list((A,B),L) :- tuple_to_list(A, La), tuple_to_list(B, Lb), append(La, Lb,L).
tuple_to_list(A,[A]) :- nonvar(A), A \= (_ , _).

list_to_tuple([H], (H)).
list_to_tuple([H|T], (H,TT)) :- list_to_tuple(T,TT).

/*
 *   Replace all the occurences of a given element with the given argument
 */
replace(_, _, [],[]).
replace(O, R, [O|T], [R|T2]) :- replace(O, R, T, T2).
replace(O, R, [H|T], [H|T2]) :- H \= O, replace(O, R, T, T2).

flatten_first_level([X], X).
flatten_first_level.

removehead([_|Tail], Tail).


defeasible_admissible([unless, Term]) :- admissible(Term).
defeasible_admissible(Term) :- admissible(Term).

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
    Term \== unless.
admissible_term(Term) :- var(Term).
admissible_term(Term) :-
    compound(Term),
    \+ functor(Term, 'o', _),
    \+ functor(Term, 'p', _),
    \+ functor(Term, '-', _),
    \+ functor(Term, '~', _),
    Term =.. [_|Args],
    admissible_terms(Args).

admissible_terms([]).
admissible_terms([H|T]) :-
    admissible_term(H),
    admissible_terms(T).


