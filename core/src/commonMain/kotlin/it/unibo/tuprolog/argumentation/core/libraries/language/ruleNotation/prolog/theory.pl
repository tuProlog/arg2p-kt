%! rules(-Rules)
%  The defeasible rules of the theory, r : B => H, as [Id, Premises, Conclusion].
rules(Rules) :-
    findall([RuleName, Preconditions, Effect], user::(RuleName : Preconditions => Effect), Rules).

%! strictRules(+Rules, +Taken, -Added)
%  The strict rules of the theory, r : B -> H.
strictRules(_, _, Added) :-
    findall([RuleName, Preconditions, Effect], user::(RuleName : Preconditions -> Effect), Added).

%! premises(-Premises)
%  The ordinary premises of the theory, f :=> a, as [Id, Statement].
premises(Premises) :-
    findall([RuleName, Effect], user::(RuleName :=> Effect), Premises).

%! axioms(-Axioms)
%  The axioms of the theory, f :-> a, as [Id, Statement].
axioms(Axioms) :-
    findall([RuleName, Effect], user::(RuleName :-> Effect), Axioms).
