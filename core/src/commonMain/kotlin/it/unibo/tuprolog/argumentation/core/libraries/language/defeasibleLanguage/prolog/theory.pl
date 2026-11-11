%! rules(-Rules)
%  The defeasible rules, as arg2p writes them.
rules(Rules) :- ruleNotation::rules(Rules).

%! check
%  The Prolog clauses read as rules (with plainProlog) are defeasible rules alone. (Premises and axioms are no part of the
%  notation of the language, and do not read.)
check :-
    parser::extension(plainProlog),
    (plainProlog::strictRules([], [], [_|_]) ; plainProlog::premises([_|_]) ; plainProlog::axioms([_|_])), !,
    throw(error(domain_error(defeasible_theory, strict), 'aspicDefeasible: the theory has strict rules, axioms or premises')).
check.
