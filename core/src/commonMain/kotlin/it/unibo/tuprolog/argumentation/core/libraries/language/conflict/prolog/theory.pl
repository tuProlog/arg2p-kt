%! between(?Statement, ?Other)
%  Statement conflicts with Other, the guard of the conflict holding.
between(Statement, Other) :-
    unguarded(Statement, Other).
between(Statement, Other) :-
    guarded(Statement, Other, Guard),
    parser::holds(Guard).

%! stated(+Conflict, ?Statement, ?Other)
%  Conflict, a conflict(A, B) or conflict(A, B, Guard) term, makes Statement ([A]) conflict with Other ([B]),
%  its guard holding.
stated(conflict(A, B), [A], [B]).
stated(conflict(A, B, Guard), [A], [B]) :-
    parser::holds(Guard).

%! declaredCounted
%  The conflict model counts the conflicts the theory declares: never with metaConflicts, where the conflicts
%  are the conclusions of arguments.
declaredCounted :-
    \+ setting(metaConflicts),
    setting(conflictModel, declared).

%! guarded(?Statement, ?Other, -Guard)
%  Statement conflicts with Other when Guard holds.
guarded(Statement, Other, Guard) :- setting(metaConflicts), !,
    Statement = [A], Other = [B],
    store::argumentFor([conflict(A, B, Guard)], _).
guarded(Statement, Other, Guard) :-
    setting(conflictModel, Part),
    builtin(Part, Statement, Other, Guard).
guarded(Statement, Other, Guard) :-
    setting(conflictModel, declared),
    declaredGuarded(Statement, Other, Guard).

%! unguarded(?Statement, ?Other)
%  Statement conflicts with Other, whatever holds.
unguarded(Statement, Other) :- setting(metaConflicts), !,
    Statement = [A], Other = [B],
    store::argumentFor([conflict(A, B)], _).
unguarded(Statement, Other) :-
    setting(conflictModel, declared),
    declaredPlainly(Statement, Other).
unguarded(Statement, Other) :-
    languageConflict(Statement, Other).

%! argued
%  The conflicts are the conclusions of arguments (metaConflicts): which statements conflict changes as the
%  graph grows, and an argument attacks through the conflicts its conclusion has with others only while their
%  arguments stand: through a copy of it carrying each (see the module conflictArguments).
argued :- setting(metaConflicts).

%! carried(+Argument, -Conflict)
%  Argument carries Conflict: the copy of an argument made for an argument for Conflict, mc(Id, Conflict) first
%  among its rules (see the module conflictArguments).
carried(Argument, Conflict) :-
    store::argumentRules(Argument, [mc(_, Conflict)|_]).

%! attacker(+Argument)
%  Argument may attack others: any argument, or, with the conflicts argued, one carrying a conflict.
attacker(Argument) :- argued, !, carried(Argument, _).
attacker(_).

%! statement(?Statement, ?Other, -ConflictStatement)
%  With metaConflicts, the arguments for ConflictStatement make Statement conflict with Other.
statement([A], [B], conflict(A, B, _)) :- setting(metaConflicts).
statement([A], [B], conflict(A, B)) :- setting(metaConflicts).

%! builtin(?Part, ?Statement, ?Other, -Guard)
%  Statement conflicts with Other when Guard holds, a conflict of the part Part of the conflict model.
% negation: a literal and its negation
builtin(negation, [Atom], [-Atom], (Atom \= -_)).
builtin(negation, [-Atom], [Atom], true).
% deontic: obligations (o/1) and permissions (p/1)
builtin(deontic, [o(Atom)], [o(-Atom)], (Atom \= -_)).          % O(x) <-> O(-x)
builtin(deontic, [o(-Atom)], [o(Atom)], true).
builtin(deontic, [p(Atom)], [o(-Atom)], (Atom \= -_)).          % P(x) <-> O(-x)
builtin(deontic, [o(-Atom)], [p(Atom)], true).
builtin(deontic, [o(Atom)], [p(-Atom)], (Atom \= -_)).          % O(x) <-> P(-x)
builtin(deontic, [p(-Atom)], [o(Atom)], true).
builtin(deontic, [o(Atom)], [-p(Atom)], true).                  % O(x) <-> O(-x) (-P(x))
builtin(deontic, [-p(Atom)], [o(Atom)], true).
% preference: a superiority and its converse
builtin(preference, [sup(X, Y)], [sup(Y, X)], true).

