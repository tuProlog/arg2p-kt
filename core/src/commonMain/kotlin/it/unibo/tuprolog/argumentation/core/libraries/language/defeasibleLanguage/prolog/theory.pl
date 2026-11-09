%! rules(-Rules)
%  The defeasible rules, as arg2p writes them.
rules(Rules) :- parser::rules(Rules).

%! check
%  The theory has defeasible rules alone.
check :-
    (parser::strictRules([], [], [_|_]) ; parser::premises([_|_]) ; parser::axioms([_|_])), !,
    throw(error(domain_error(defeasible_theory, strict), 'aspic_defeasible: the theory has strict rules, axioms or premises')).
check :-
    parser::extension(plainProlog),
    (plainProlog::strictRules([], [], [_|_]) ; plainProlog::premises([_|_]) ; plainProlog::axioms([_|_])), !,
    throw(error(domain_error(defeasible_theory, strict), 'aspic_defeasible: the theory has strict rules, axioms or premises')).
check.
