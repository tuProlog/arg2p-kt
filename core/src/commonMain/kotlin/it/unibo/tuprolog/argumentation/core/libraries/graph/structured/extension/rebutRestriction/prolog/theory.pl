%! stepScope(-Scope)
%  own: the step goes through the attacks or the arguments it changes each alone, which the solvers of a session
%  building the graph together divide (see phases::stepScope/2).
stepScope(own).

%! modifyArgumentationGraph
%  The restricted attacks on an argument whose top rule is strict are removed.
modifyArgumentationGraph :-
    findall(a(T, IdA, IdC), (
        store::attackById(T, IdA, IdC, IdC),
        store::argumentById(IdC, C),
        \+ rebutRestriction(T, C)
    ), Restricted),
    store::removeAttackIds(Restricted).

% The attack types the restriction does not apply to. Everything else is restricted: rebuts, and the attacks
% of a graph extension of the user.
rebutRestriction(undermine, _).
rebutRestriction(contrary_rebut, _).
rebutRestriction(contrary_undermine, _).
rebutRestriction(undercut, _).
% the top rule of the argument is its last defeasible rule (see attackRelation::defeasibleTopRule/2)
rebutRestriction(_, C) :-
    store::defeasibleTopRule(C, _).
