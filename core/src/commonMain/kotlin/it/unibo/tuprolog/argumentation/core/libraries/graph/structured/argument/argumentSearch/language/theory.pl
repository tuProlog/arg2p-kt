%! boundByQuery(?Id)
%  A query could bind the arguments of the rule or premise Id otherwise than the whole graph derives them: its
%  conclusion, or a Prolog check, has a variable its ordinary premises written before leave unbound (see boundByQuery/2).
%  Read the first time it is asked for.
boundByQuery(Id) :-
    utils::oncePerTheory(boundByQuery, argumentSearch::readBoundByQuery),
    cache_check(boundByQueryRule(Id)).

readBoundByQuery :- \+ \+ readBoundByQuery(_).

readBoundByQuery(Rules) :-
    parser::writtenRules(Rules),
    boundByQuery(Rules, Ids),
    findall(_, (member(Id, Ids), cache_assert(boundByQueryRule(Id))), _).

%! boundByQuery(+Rules, -Ids)
%  Of Rules, as read from the theory ([Id, Premises, Conclusion], the
%  premises as parser::rule/3 reads them, and [Id, Conclusion] for a premise of the theory), Ids are those whose
%  arguments a query could bind otherwise than the whole graph derives them: a variable of the conclusion that no
%  ordinary premise has, or one that a Prolog check (prolog(Goal)) uses before the ordinary premises written before
%  it bind it (X is Expr binds X). For the others the search steered by a query finds the arguments the whole graph
%  has; contraries and constraints ({ C }) bind nothing.
boundByQuery([], []).
boundByQuery([Rule|Rules], Ids) :-
    boundByQuery(Rules, OtherIds),
    parser::ruleParts(Rule, Id, Premises, Conclusion), !,
    (\+ \+ bindsMore(Premises, Conclusion) -> Ids = [Id|OtherIds] ; Ids = OtherIds).
boundByQuery([_|Rules], Ids) :-
    boundByQuery(Rules, Ids).

% The variables are bound as they are met, what is ground is what the premises so far bind (asked within \+ \+)
bindsMore([], Conclusion) :- \+ ground(Conclusion).
bindsMore([Premise|Premises], Conclusion) :-
    parser::premiseKind(Premise, Kind),
    bindsMoreAs(Kind, Premise, Premises, Conclusion).

bindsMoreAs(check(Goal), _, Premises, Conclusion) :- !,
    (checked(Goal) -> bindsMore(Premises, Conclusion) ; true).
bindsMoreAs(statement, Premise, Premises, Conclusion) :- !,
    bindAll(Premise),
    bindsMore(Premises, Conclusion).
bindsMoreAs(_, _, Premises, Conclusion) :-
    bindsMore(Premises, Conclusion).

% A check has its variables bound when it runs: a conjunction in its order, X is Expr binding X
checked(Goal) :- nonvar(Goal), Goal = (A, B), !, checked(A), checked(B).
checked(Goal) :- nonvar(Goal), Goal = (X is Expr), var(X), !, ground(Expr), bindAll(X).
checked(Goal) :- ground(Goal).

bindAll(Term) :- var(Term), !, Term = bound.
bindAll(Term) :- atomic(Term), !.
bindAll(Term) :- Term =.. [_|Args], bindEach(Args).
bindEach([]).
bindEach([Arg|Args]) :- bindAll(Arg), bindEach(Args).
