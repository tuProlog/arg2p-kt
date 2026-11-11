%! preferenceMode
%  This graph extension weighs the attacks by the preferences (see pipeline::preferenceMode/1).
preferenceMode.

%! stepScope(-Scope)
%  own: the step goes through the attacks or the arguments it changes each alone, which the solvers of a session
%  building the graph together divide (see pipeline::stepScope/2).
stepScope(own).

%! modifyArgumentationGraph
%  The attacks failing for the preferences are removed. Attacks are visited by argument identifiers, and
%  arguments are compared by their info only.
modifyArgumentationGraph :-
    findall(a(T, IdA, IdC), (
        store::attackById(T, IdA, IdC, IdC),
        failing(T, IdA, IdC)
    ), Failing),
    store::removeAttackIds(Failing).

failing(T, IdA, IdC) :- standardPreferences(T, IdA, IdC), !, fail.
failing(_, _, _).

% The attack types preferences do not apply to. Everything else is weighed against the preference order: rebuts
% and undermines, and the attacks of a graph extension of the user.
standardPreferences(contraryRebut, _, _) :- !.
standardPreferences(contraryUndermine, _, _) :- !.
standardPreferences(undercut, _, _) :- !.
standardPreferences(_, IdA, IdC) :-
    store::argumentInfoId(IdA, InfoIdA),
    store::argumentInfoId(IdC, InfoIdC),
    superiority::superiorById(InfoIdC, InfoIdA), !,
    fail.
standardPreferences(_, _, _).
