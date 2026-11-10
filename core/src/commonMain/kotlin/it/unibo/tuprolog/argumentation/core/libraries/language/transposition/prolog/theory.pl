%! strictRules(+Rules, +Taken, -Added)
%  Added are the transpositions of the strict rules Rules. Taken are the names of the rules and premises of the theory,
%  which no transposition takes (see suffixed/3). From the last rule to the first, keeping the last transposition with
%  each name: what merging the transpositions of a rule at a time, and deduplicating each time, amounts to, in a single
%  pass rather than merging and deduplicating the lists once for each rule (cubic in the number of rules).
strictRules(Rules, Taken, Added) :-
    findall(_, (member(Name, Taken), atom(Name), context_assert(transpositionTaken(Name))), _),
    reverse(Rules, Reversed),
    findall(TrH, (member(H, Reversed), transposition(H, TrH)), Tran),
    utils::uniqueByFirst(Tran, Added),
    context_retract(transpositionTaken(_)).

transposition([Id, Prec, Effect], [NewId, NewPrec ,XNegated]) :-
    compound(Prec),
    utils::tupleToList(Prec, LPrec),!,
    transpositionSequential(LPrec, LPrec, Effect, Id, [], NewPrec, XNegated, NewId).
transposition([Id, Prec, Effect], [NewId, EffectNegated ,XNegated]) :-
    \+ compound(Prec),
    Prec \== [],
    transposable(Prec),
    contradictories(Prec, Effect, XNegated, EffectNegated, Kind),
    suffixed(Id, '_i', BaseId),
    named(Kind, BaseId, [EffectNegated, XNegated], NewId).

transpositionSequential(LPrec, [H|T], Effect, Id, Skipped, NewPrec, XNegated, NewId) :-
    transpositionSequential(LPrec, T, Effect, Id, [H|Skipped], NewPrec, XNegated, NewId).
transpositionSequential(LPrec, [X|T], Effect, Id, Skipped, NewPrec, XNegated, NewId) :-
    transposable(X),
    % the premises before X in the order they are written (Skipped has them reversed): a Prolog check among them
    % comes after the premises binding its variables, as in the rule
    reverse(Skipped, Before),
    append(Before, T, CleanedPrec),
    contradictories(X, Effect, XNegated, EffectNegated, Kind),
    listToTuple([EffectNegated|CleanedPrec], NewPrec),
    newIdentifier(Skipped, Id, BaseId),
    named(Kind, BaseId, [EffectNegated, XNegated], NewId).

% The contradictories of a premise and of the conclusion of a rule: their negations (negation), or a pair of
% which one at least is declared in conflict both ways (declared)
contradictories(X, Effect, XNegated, EffectNegated, negation) :-
    negate(X, XNegated),
    negate(Effect, EffectNegated).
contradictories(X, Effect, XNegated, EffectNegated, declared) :-
    contradictory(X, XNegated),
    contradictory(Effect, EffectNegated),
    (bothWays(X, XNegated) ; bothWays(Effect, EffectNegated)).

contradictory(X, Negated) :- negate(X, Negated).
contradictory(X, Other) :- bothWays(X, Other).

% X and Other declared in conflict both ways by the theory
bothWays(X, Other) :- conflict::declaredBothWays(X, Other).

% The name of a transposition: through negations, the one of the rule with its suffix
named(negation, Id, _, Id).
named(declared, Id, Contradictories, transposed(Id, Contradictories)).

newIdentifier(List, OldId, NewId) :-
    modifier(List, Mod),
    suffixed(OldId, Mod, NewId).

% The identifier of a transposition: the one of the rule with a suffix, transposition(Id, Suffix) for a rule
% whose identifier is no atom (a rule with parameters, t(X, Y)) or when a rule or premise of the theory has the
% name with the suffix (transpositionTaken/1): a transposition never takes the place of a rule of the theory
suffixed(Id, Suffix, NewId) :- atom(Id), atom_concat(Id, Suffix, NewId), context_absent(transpositionTaken(NewId)), !.
suffixed(Id, Suffix, transposition(Id, Suffix)).

modifier([], '_i').
modifier([_|Tail], Index):-
    modifier(Tail, Index1),
    atom_concat(Index1, 'i', Index).

% The negation of a statement: -X of X, X of -X, and the obligations of permissions (o(-X) of p(X), o(X) of
% p(-X)); the terms are matched as they are, not unified
negate(X, Arg) :- nonvar(X), X = -(A), !, Arg = A.
negate(X, Arg) :- nonvar(X), X = p(PX), !,
    (nonvar(PX), PX = -(PNX) -> Arg = o(PNX) ; Arg = o(-(PX))).
negate(X, -(X)).

% The premises transposed: the statements (see rule::premiseKind/2), not the body true of a fact
transposable(X) :- X \== true, store::premiseKind(X, statement).

listToTuple([H], (H)) :- !.
listToTuple([H|T], (H,TT)) :- listToTuple(T,TT).
