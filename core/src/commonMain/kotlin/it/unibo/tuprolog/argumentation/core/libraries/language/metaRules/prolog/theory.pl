%! rewrite(+Rules, +Ids, -Rewritten, -Ids)
%  The rules read (see parser::rewritten/4), each rule Id with applicable(Id), a statement of the theory like any other,
%  before the premises written: a rule is applied only where an argument concludes that it applies (or its premise is
%  stated). The premises of the theory ([Id, Statement]) and the strict/1 facts stay as they are.
rewrite(Rules, Ids, Rewritten, Ids) :-
    applicableFirst(Rules, Rewritten).

applicableFirst([], []).
applicableFirst([[Id, Premises, Conclusion]|Rules], [[Id, WithApplicable, Conclusion]|Rewritten]) :- !,
    withApplicable(Id, Premises, WithApplicable),
    applicableFirst(Rules, Rewritten).
applicableFirst([Premise|Rules], [Premise|Rewritten]) :-
    applicableFirst(Rules, Rewritten).

% The premises written (a conjunction; none, [], or one, [P], as a list) with applicable(Id) before them
withApplicable(Id, [], applicable(Id)) :- !.
withApplicable(Id, [Premise], (applicable(Id), Premise)) :- !.
withApplicable(Id, Premises, (applicable(Id), Premises)).
