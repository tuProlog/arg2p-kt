%! strictRules(+Rules, +Taken, -Added)
%  The strict rules, as arg2p writes them.
strictRules(Rules, Taken, Added) :- parser::strictRules(Rules, Taken, Added).

%! axioms(-Axioms)
%  The axioms, as arg2p writes them.
axioms(Axioms) :- parser::axioms(Axioms).

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
%  The theory is flat ABA: no defeasible rule, no ordinary premise (its premises are its assumptions, its facts
%  strict), and no rule concluding an assumption.
check :-
    (parser::rules([_|_]) ; parser::extension(plainProlog), plainProlog::rules([_|_])), !,
    throw(error(domain_error(aba_theory, defeasible), 'aba: the theory has defeasible rules')).
check :-
    ordinaryPremise, !,
    throw(error(domain_error(aba_theory, premise), 'aba: the theory has ordinary premises (f :=> a.): the premises of ABA are its assumptions (assumption/1), its facts strict (f :-> a.)')).
check :-
    parser::rule(_, _, [A]),
    assumption(Id, A),
    parser::premise(Id, [A]), !,
    throw(error(domain_error(flat_aba_theory, A), 'aba: a rule concludes an assumption (the theory is not flat)')).
% (no preferences between rules, nor the meta-argumentation of ASPIC+)
check :-
    setting(graphExtension, M),
    phases::preferenceMode(M),
    M \== abaPref, !,
    throw(error(domain_error(aba_theory, M), 'graphExtension: an ABA theory has no preferences between rules (standardPref, defeasiblePref, defeasibleAllPref): abaPref for ABA+')).
check :-
    (setting(metaConflicts) -> Meta = metaConflicts ; setting(metaRules) -> Meta = metaRules), !,
    throw(error(domain_error(aba_theory, Meta), 'aba: the meta-argumentation of ASPIC+ (metaConflicts, metaRules) does not apply to an ABA theory')).
check.

% A premise of the theory other than an assumption written out (asm(A) :=> A, as a partition of a theory writes it)
ordinaryPremise :-
    parser::premises(Premises),
    member([Id, _], Premises),
    \+ assumption(Id, _), !.
ordinaryPremise :-
    parser::extension(plainProlog),
    plainProlog::premises([_|_]).
