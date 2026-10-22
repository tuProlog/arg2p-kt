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

transpose(_, CtrRules, CtrRules) :- \+ setting(autoTransposition), !.
% The rules followed by the transpositions of each one, from the last rule to the first, keeping the last rule
% with each name: what merging the transpositions of a rule at a time, and deduplicating each time, amounts to,
% in a single pass rather than merging and deduplicating the lists once for each rule (cubic in the number of
% rules).
transpose(Rules, TempCtrRules, CtrRules) :-
    reverse(Rules, Reversed),
    findall(TrH, (member(H, Reversed), transposition(H, TrH)), Tran),
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

list_to_tuple([H], (H)) :- !.
list_to_tuple([H|T], (H,TT)) :- list_to_tuple(T,TT).


rule(Id, PP, [Conclusion]) :- context_check(clause(rl(Conclusion), [Id, Premises, Conclusion])), tuple_to_list(Premises, P), ff(P, PP).
premise(Id, [Conclusion]) :- context_check(clause(rl(Conclusion), [Id, Conclusion])).

% candidates(+Statement, +Limit, -Count): the number of rules and premises concluding Statement, counted up to
% Limit (an integer, or anything else for no limit)
candidates(Statement, Limit, Count) :- context_count(clause(rl(Statement), _), Limit, Count).


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
classic_rule(X, [applicable(X)|Y], Z) :- setting(metaRules), !, rule(X, Y, Z).
classic_rule(X, Y, Z) :- rule(X, Y, Z).
classic_rule(X, Y) :- premise(X, Y).

% -P(x) <=> O(-x)
% -O(x) <=> P(-x)  % this is already included

% With metaConflicts the conflicts are only the derived ones, otherwise the builtin and declared ones
classic_conflict(X, Y, C) :- setting(metaConflicts), !,
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
classic_conflict(X, Y) :- setting(metaConflicts), !,
    X = [A], Y = [B],
    context_check(clause(conc([conflict(A, B)]), _)).
classic_conflict(A, B) :-
    conflict(A, B).

% conflict_statement(?X, ?Y, -Statement): with metaConflicts, the statements whose arguments make X conflict with Y
conflict_statement([A], [B], conflict(A, B, _)) :- setting(metaConflicts).
conflict_statement([A], [B], conflict(A, B)) :- setting(metaConflicts).
