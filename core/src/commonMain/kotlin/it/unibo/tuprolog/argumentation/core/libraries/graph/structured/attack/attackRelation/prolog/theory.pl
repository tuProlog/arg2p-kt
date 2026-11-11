%! attacks(?Type, +Attacker, +Attacked)
%  Attacker attacks Attacked directly, Type being the first kind of attack that applies. With metaConflicts
%  every attack goes through a conflict concluded by an argument (see the module conflictArguments): the
%  contraries and the undercuts by undercut(Rule), which no such conflict states, do not apply (see static/0).
attacks(rebut, A, B) :- rebuts(A, B), !.
attacks(contraryRebut, A, B) :- contraryRebuts(A, B), !.
attacks(undermine, A, B) :- undermines(A, B), !.
attacks(contraryUndermine, A, B) :- contraryUndermines(A, B), !.
attacks(undercut, A, B) :- undercuts(A, B), !.

%! rebuts(+A, +B)
%  The conclusion of A conflicts with the one of B, a defeasible argument with a top rule.
rebuts(A, B) :-
    \+ store::isPremise(B),
    store::isDefeasible(B),
    store::conclusion(B, Conclusion),
    conflictsWith(A, Conclusion).

%! contraryRebuts(+A, +B)
%  A concludes the contrary of a premise of the top rule of B (~(Claim)), both having a top rule.
contraryRebuts(A, B) :-
    \+ store::isPremise(A),
    \+ store::isPremise(B),
    store::claim(A, Claim),
    store::body(B, Body),
    utils::contains(~(Claim), Body),
    static.

%! undermines(+A, +B)
%  The conclusion of A conflicts with B, a defeasible premise (or its copy for a conflict, see the module
%  conflictArguments).
undermines(A, B) :-
    store::isPremise(B),
    store::isDefeasible(B),
    store::conclusion(B, Conclusion),
    conflictsWith(A, Conclusion).

%! contraryUndermines(+A, +B)
%  A, a premise, is the contrary of a premise of the top rule of B (~(Claim)).
contraryUndermines(A, B) :-
    store::isPremise(A),
    \+ store::isPremise(B),
    store::claim(A, Claim),
    store::body(B, Body),
    utils::contains(~(Claim), Body),
    static.

%! undercuts(+A, +B)
%  A concludes undercut(Rule), or a statement conflicting with [Rule], Rule being the defeasible top rule of B.
undercuts(A, B) :-
    store::defeasibleTopRule(B, Rule),
    store::claim(A, undercut(Rule)),
    static.
undercuts(A, B) :-
    store::defeasibleTopRule(B, Rule),
    conflictsWith(A, [Rule]).

%! static
%  The attacks no conflict states apply (contraries, undercut(Rule)): unless the conflicts are concluded by
%  arguments (see conflict::argued/0).
static :- \+ conflict::argued.


%! conflictsWith(+Argument, ?Statement)
%  The conclusion of Argument conflicts with Statement: only through the conflict Argument carries if it does
%  (see conflict::carried/2), through any conflict otherwise.
conflictsWith(Argument, Statement) :-
    conflict::carried(Argument, Conflict), !,
    store::conclusion(Argument, Conclusion),
    conflict::stated(Conflict, Conclusion, Statement).
conflictsWith(Argument, Statement) :-
    store::conclusion(Argument, Conclusion),
    conflict::between(Conclusion, Statement).
