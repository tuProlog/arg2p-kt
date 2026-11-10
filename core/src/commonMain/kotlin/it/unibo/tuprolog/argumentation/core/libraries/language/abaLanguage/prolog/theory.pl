%! strictRules(+Rules, +Taken, -Added)
%  The strict rules, as arg2p writes them.
strictRules(Rules, Taken, Added) :- ruleNotation::strictRules(Rules, Taken, Added).

%! axioms(-Axioms)
%  The axioms, as arg2p writes them.
axioms(Axioms) :- ruleNotation::axioms(Axioms).

%! premises(-Premises)
%  The assumptions, as ordinary premises [asm(Assumption), Assumption].
premises(Premises) :-
    findall([Id, A], (user::assumption(A), assumption(Id, A)), Premises).

%! assumption(?Id, ?Assumption)
%  The premise Id is the assumption Assumption (asm(Assumption)).
assumption(asm(Assumption), Assumption).

%! conflicts(-Conflicts)
%  The contraries, as the conflicts conflict([Contrary], [Assumption]).
conflicts(Conflicts) :-
    findall(conflict([C], [A]), user::contrary(A, C), Conflicts).

%! check
%  The theory is flat ABA: no rule concluding an assumption. (A defeasible rule or an ordinary premise is no part of its
%  notation, and does not read; the Prolog clauses read as rules, with plainProlog, are checked here.)
check :-
    parser::extension(plainProlog),
    plainProlog::rules([_|_]), !,
    throw(error(domain_error(aba_theory, defeasible), 'aba: the theory has defeasible rules')).
check :-
    ordinaryPremise, !,
    throw(error(domain_error(aba_theory, premise), 'aba: the theory has ordinary premises (f :=> a.): the premises of ABA are its assumptions (assumption/1), its facts strict (f :-> a.)')).
check :-
    store::rule(_, _, [A]),
    assumption(Id, A),
    store::premise(Id, [A]), !,
    throw(error(domain_error(flat_aba_theory, A), 'aba: a rule concludes an assumption (the theory is not flat)')).
% (no preferences between rules, nor the meta-argumentation of ASPIC+)
check :-
    setting(graphExtension, M),
    module_defines(M, preferenceMode/0),
    M \== abaPref, !,
    throw(error(domain_error(aba_theory, M), 'graphExtension: an ABA theory has no preferences between rules (standardPref, defeasiblePref, defeasibleAllPref): abaPref for ABA+')).
check :-
    (setting(metaConflicts) -> Meta = metaConflicts ; setting(metaRules) -> Meta = metaRules), !,
    throw(error(domain_error(aba_theory, Meta), 'aba: the meta-argumentation of ASPIC+ (metaConflicts, metaRules) does not apply to an ABA theory')).
check.

% A Prolog clause of the theory read as an ordinary premise (with plainProlog)
ordinaryPremise :-
    parser::extension(plainProlog),
    plainProlog::premises([_|_]).
