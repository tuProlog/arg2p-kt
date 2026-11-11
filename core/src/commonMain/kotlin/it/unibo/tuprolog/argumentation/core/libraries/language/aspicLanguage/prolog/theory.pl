%! rules(-Rules)
%  The defeasible rules, as arg2p writes them.
rules(Rules) :- ruleNotation::rules(Rules).

%! strictRules(+Rules, +Taken, -Added)
%  The strict rules, as arg2p writes them.
strictRules(Rules, Taken, Added) :- ruleNotation::strictRules(Rules, Taken, Added).

%! premises(-Premises)
%  The ordinary premises, as arg2p writes them.
premises(Premises) :- ruleNotation::premises(Premises).

%! axioms(-Axioms)
%  The axioms, as arg2p writes them.
axioms(Axioms) :- ruleNotation::axioms(Axioms).

%! check
%  The theory is well formed: no contrary of an axiom or of the conclusion of a strict rule.
check :-
    contrary(Contrary, Statement),
    strictly(Statement), !,
    throw(error(domain_error(well_formed_theory, Statement), 'aspic: a contrary (a conflict declared one way) targets an axiom or the conclusion of a strict rule')).
check.

% Contrary conflicts with Statement one way only, as the theory declares (see conflict::declared/2)
contrary(Contrary, Statement) :-
    conflict::declared([Contrary], [Statement]),
    \+ conflict::declared([Statement], [Contrary]).

% Statement is an axiom, or the conclusion of a strict rule
strictly(Statement) :- store::premise(Id, [Statement]), store::checkStrict(Id).
strictly(Statement) :- store::rule(Id, _, [Statement]), store::checkStrict(Id).
