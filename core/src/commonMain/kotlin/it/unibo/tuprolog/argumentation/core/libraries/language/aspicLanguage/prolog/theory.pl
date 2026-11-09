%! rules(-Rules)
%  The defeasible rules, as arg2p writes them.
rules(Rules) :- parser::rules(Rules).

%! strictRules(+Rules, +Taken, -Added)
%  The strict rules, as arg2p writes them.
strictRules(Rules, Taken, Added) :- parser::strictRules(Rules, Taken, Added).

%! premises(-Premises)
%  The ordinary premises, as arg2p writes them.
premises(Premises) :- parser::premises(Premises).

%! axioms(-Axioms)
%  The axioms, as arg2p writes them.
axioms(Axioms) :- parser::axioms(Axioms).

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
strictly(Statement) :- parser::premise(Id, [Statement]), parser::checkStrict(Id).
strictly(Statement) :- parser::rule(Id, _, [Statement]), parser::checkStrict(Id).
