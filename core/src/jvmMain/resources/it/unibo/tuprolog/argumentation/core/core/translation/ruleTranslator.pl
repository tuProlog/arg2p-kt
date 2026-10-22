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
    utils::appendLists([AxiomsIds, RulesIds], StrictIds),
    utils::appendLists([DefeasibleRules, StrictRules, Premises, Axioms], Rules),
    naturalRules(Rules, Clauses, StrictIds, MappedIds),
    findall(sup(X, Y), sup(X, Y), Sup),
    utils::appendLists([Clauses, MappedIds, Sup, SpecialRules], ArgRules),
    utils::assert_all(ArgRules), !.

naturalRules(Rules, Clauses, StrictIds, MappedIds) :-
    naturalTerms,
    rule_to_clause_natural(Rules, Clauses),
    mapIds(Rules, Clauses, StrictIds, MappedIds).
naturalRules(Rules, Clauses, StrictIds, StrictIds) :-
    \+ naturalTerms,
    rule_to_clause(Rules, Clauses).

recoverId(rl(_) :- [Y, _, _], Y).
recoverId(rl(_) :- [Y, _], Y).
recoverId([Y, _, _], Y).
recoverId([Y, _], Y).

mapIds([], [], _, []).
mapIds([W|T], [Z|TT], Ids, [strict(Y)|RR]) :-
    once(recoverId(W, X)),
    member(strict(X), Ids), !,
    once(recoverId(Z, Y)),
    mapIds(T, TT, Ids, RR).
mapIds([_|T], [_|TT], Ids, RR) :-
    mapIds(T, TT, Ids, RR).

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
% The rules followed by the transpositions of each one, from the last rule to the first, keeping the last rule
% with each name: what merging the transpositions of a rule at a time, and deduplicating each time, amounts to,
% in a single pass rather than in a recursion over the rules (cubic, carrying the lists in every frame).
% The rules are visited from the context (the newest first, so from the last): with variable tracking, a list
% of them in the goal of the findall would make every step as costly as the variables of all the rules.
transpose(Rules, TempCtrRules, CtrRules) :-
    findall(transposable(Rule), utils::element(Rule, Rules), Transposable),
    context_assert_all(Transposable),
    findall(TrH, (context_check(transposable(H)), transposition(H, TrH)), Tran),
    context_retract(transposable(_)),
    utils::append_fast(TempCtrRules, Tran, All),
    utils::unique_by_first(All, CtrRules).

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
check_modifiers_in_list(MODE, [H|T], R) :- H \== [],
                            check_modifiers_once(H, LH),
                            % check_admissibility(MODE, H, LH),
                            check_modifiers_in_list(MODE, T, LT),
                            utils::append_fast([LH], LT, L),
                            R = L.

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



%%%%%%%%%%%%%%%%%%%%%%%% REFACTOR %%%%%%%%%%%%%%%%%%%%%%%



rule(Id, PP, [Conclusion]) :- context_check(clause(rl(Conclusion), [Id, Premises, Conclusion])), tuple_to_list(Premises, P), ff(P, PP).
premise(Id, [Conclusion]) :- context_check(clause(rl(Conclusion), [Id, Conclusion])).


check_strict(Id) :- context_check(strict(Id)).

prolog([prolog(Term)], Term).
contrary([~(Term)], Term).
undercut(undercut(Rule), Rule).
superiority(sup(Id1, Id2), Id1, Id2).

/*
 *   Convert the given tuple to list
 */
% The items of a conjunction, as a list (built with an accumulator rather than appending the lists of the two
% sides, which took a module call at every step)
tuple_to_list(T, L) :- tuple_to_list(T, L, []).
tuple_to_list(A, [A|R], R) :- var(A), !.
tuple_to_list((A, B), L, R) :- !, tuple_to_list(A, L, M), tuple_to_list(B, M, R).
tuple_to_list(A, [A|R], R).

ff([[]], []) :- !.
ff([[X]], [X]) :- !.
ff(X, X).

% classic_rule(mc, [conflict(A, B), A], [A]) :- metaConflicts.
classic_rule(X, [applicable(X)|Y], Z) :- metaRules, !, rule(X, Y, Z).
classic_rule(X, Y, Z) :- rule(X, Y, Z).
classic_rule(X, Y) :- premise(X, Y).

% -P(x) <=> O(-x)
% -O(x) <=> P(-x)  % this is already included

% With metaConflicts the conflicts are only the derived ones, otherwise the builtin and declared ones
classic_conflict(X, Y, C) :- metaConflicts, !,
    X = [A], Y = [B],
    context_check(clause(conc([conflict(A, B, C)]), _)).
classic_conflict(A, B, C) :-
    builtin_conflict(A, B, C).
classic_conflict(A, B, C) :-
    conflict(A, B, C).

builtin_conflict([Atom], [-Atom], (Atom \= -_)).
builtin_conflict([-Atom], [Atom], true).
builtin_conflict([o(Atom)], [o(-Atom)], (Atom \= -_)).                   % O(x) <-> O(-x)
builtin_conflict([o(-Atom)], [o(Atom)], true).
builtin_conflict([p(Atom)], [o(-Atom)], (Atom \= -_)).                   % P(x) <-> O(-x)
builtin_conflict([o(-Atom)], [p(Atom)], true).
builtin_conflict([o(Atom)], [p(-Atom)], (Atom \= -_)).                   % O(x) <-> P(-x)
builtin_conflict([p(-Atom)], [o(Atom)], true).
builtin_conflict([o(Atom)], [-p(Atom)], true).                           % O(x) <-> O(-x) (-P(x))
builtin_conflict([-p(Atom)], [o(Atom)], true).
builtin_conflict([sup(X, Y)], [sup(Y, X)], true).
classic_conflict(X, Y) :- metaConflicts, !,
    X = [A], Y = [B],
    context_check(clause(conc([conflict(A, B)]), _)).
classic_conflict(A, B) :-
    conflict(A, B).
