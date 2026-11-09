%! buildAttacks
%  The direct attacks between the saved arguments, an argument attacking itself included (as r : a => undercut(r)
%  does): those on the arguments built on the attacked one follow from them (see the module store). Loops
%  over the graph go through its copy by identifiers. None when the conflicts are arguments (see
%  conflict::argued/0): only their copies attack, added when the graph is changed (see the module
%  conflictArguments).
buildAttacks :-
    conflict::argued, !.
buildAttacks :-
    (constraints::constrained -> Constrained = yes ; Constrained = no),
    findall(_, buildDirectAttack(Constrained), _).

% (Constrained: the theory may have constraints, which the two arguments must satisfy together with the
% bindings of their conflict, see constraints::compatible/2)
buildDirectAttack(Constrained) :-
    store::argumentById(IdA, A),
    attackCandidates::attackersOf(IdA, A, Conflict),
    findAttacker(Conflict, B),
    attackAs(Constrained, T, B, A),
    store::argumentId(B, IdB),
    store::saveAttackById(T, IdB, B, IdA, A, IdA, A),
    fail.
buildDirectAttack(_).

%! buildAttacksOf(+Ids)
%  The direct attacks on and by the saved arguments with identifiers Ids, added to a graph with its attacks
%  (those between the others are saved already).
%  The solvers of a session changing a graph together each find them on their own arguments (see the module
%  store), once every one added its arguments (graph_sync/1: those the others added, none alone): every
%  step calling it calls it once.
buildAttacksOf(Own) :-
    store::graph_sync(Others),
    append(Own, Others, Ids),
    buildAttacksAmong(Ids).

buildAttacksAmong([]) :- !.
buildAttacksAmong(Ids) :-
    findall(_, (member(Id, Ids), context_assert(addedArgument(Id))), _),
    (constraints::constrained -> Constrained = yes ; Constrained = no),
    findall(_, buildAddedAttack(Constrained), _),
    context_retract(addedArgument(_)).

% (from every argument to its attackers, not from those added to what they attack: a solver of a shared graph has its
% own rules alone, which that way goes through)
buildAddedAttack(Constrained) :-
    store::argumentById(IdA, A),
    attackCandidates::possibleAttackers(IdA, A, Conflict),
    findAttacker(Conflict, B),
    store::argumentId(B, IdB),
    (context_check(addedArgument(IdA)) -> true ; context_check(addedArgument(IdB))),
    attackAs(Constrained, T, B, A),
    store::saveAttackById(T, IdB, B, IdA, A, IdA, A),
    fail.
buildAddedAttack(_).

% (Constrained: whether the theory may have constraints, found once for all the attacks of a graph.) Attacks are
% checked on copies, so that the arguments are saved exactly as they are stored: binding their variables would
% change their identifiers (see context_id)
attackAs(Constrained, T, B, A) :-
    conflict::attacker(B),
    copy_term(B-A, CB-CA),
    attackRelation::attacks(T, CB, CA),
    (Constrained == yes -> constraints::compatible(CB, CA) ; true).

% Finds the stored arguments whose conclusion unifies with the given one, without instantiating them
findAttacker([Head], B) :- !,
    store::argumentUnifiable([Head], B).
findAttacker(Conclusion, B) :-
    store::argumentFor(Conclusion, B).
