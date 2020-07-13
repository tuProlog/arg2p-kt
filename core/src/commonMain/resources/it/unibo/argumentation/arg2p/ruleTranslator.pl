% ----------------------------------------------------------------
% ruleTranslator.pl
% PIKA-lab
% Year: 2019
% -------------------------------------------------------------------------------

% rule_name : o(-what_is_mandatory_to_avoid), o(what_is_mandatory), p(what_is_permitted), p(-what_is_permitted), what_is_true, -what_is_true => effect.
% are converted into lists expressing rules
% rule([v, [ [obl, [neg, enter]], [enter] ], [violation] ]).
% rule([rPerm, [ [emer] ], [perm, [enter]] ]).

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
    retractall(rule(_)), !,
    retractall(abstractBp(_)), !,
    retractall(reifiedBp(_)), !,
    findall([RuleName, Preconditions, Effect], (RuleName : Preconditions => Effect), StandardRules),
    findall([_, X], search('bp', 10, X), SpecialRules),
    append(StandardRules, SpecialRules, L),
    convertAllRules(L).

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
                            check_modifiers_in_list(Lprecond, LprecondChecked),
                            check_modifiers_in_list(Leffects, LeffectsChecked),
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
            functor(Effects, 'bp', _) ->
                Effects =.. L,
                removehead(L, LC),
                check_modifiers_in_list(LC, Checked),
                assert(abstractBp(Checked));
            true.

%=======================================================================================================================

/*
 *   Find negations(-), obligations(o), permissions(p) on a list of preconditions/effects and
 *   raplace them with the assigned literal (neg, obl, perm)
 */
check_modifiers_in_list([], []).
check_modifiers_in_list([H|T], L) :- H == [], L = [].
check_modifiers_in_list([H|T], L) :- H \== [],
                            check_modifiers(H, LH),
                            check_modifiers_in_list(T, LT),
                            append([LH], LT, L).

check_modifiers([], []).
check_modifiers(H, List) :-	functor(H, '-', _) -> H =.. L, replace('-', 'neg', L, Lf), List = Lf;
                            functor(H, 'o', _) -> (arg(1, H, Arg), check_modifiers(Arg, Lobl), List = ['obl'|[Lobl]]);
                            functor(H, 'p', _) -> (arg(1, H, Arg), check_modifiers(Arg, Lper), List = ['perm'|[Lper]]);
                            List = [H].

/*
 *   Convert the given tuple to list
 */
tuple_to_list((A,B),L) :- tuple_to_list(A, La), tuple_to_list(B, Lb), append(La, Lb,L).
tuple_to_list(A,[A]) :- nonvar(A), A \= (_ , _).

/*
 *   Replace all the occurences of a given element with the given argument
 */
replace(_, _, [],[]).
replace(O, R, [O|T], [R|T2]) :- replace(O, R, T, T2).
replace(O, R, [H|T], [H|T2]) :- H \= O, replace(O, R, T, T2).

flatten_first_level([X], X).
flatten_first_level.

removehead([_|Tail], Tail).