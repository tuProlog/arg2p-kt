%! preferenceMode
%  This graph extension weighs the attacks by the preferences (see pipeline::preferenceMode/1).
preferenceMode.

%! stepScope(-Scope)
%  own: the step goes through the attacks or the arguments it changes each alone, which the solvers of a session
%  building the graph together divide (see pipeline::stepScope/2).
stepScope(own).

%! modifyArgumentationGraph
%  The attacks on assumptions by arguments resting on weaker ones reversed, the theory satisfying Weak
%  Contraposition.
modifyArgumentationGraph :-
    store::directAttacks(All),
    findall(attack(undermine, X, On, On), member((undermine, X, On), All), Attacks),
    utils::forEach(attack(_, X, _, On), Attacks, abaPref::contraposed(X, On)),
    utils::forEach(attack(T, X, Y, On), Attacks, abaPref::reverse(T, X, Y, On)).

%! contraposed(+Attacker, +On)
%  Weak Contraposition (Čyras and Toni 2016, Axiom 4), on which the reversal of the attacks between arguments
%  is that of ABA+ between sets of assumptions: if Attacker, resting on the assumptions Support, attacks the
%  assumption Beta though some are weaker than it, then for one Alpha of the weakest of them an argument
%  attacks Alpha resting on Support but Alpha, and Beta. A theory failing it is refused.
contraposed(X, On) :-
    \+ restsOnWeaker(X, On), !.
contraposed(X, On) :-
    store::claim(On, Beta),
    assumptionsOf(X, Support),
    findall(A, (member(A, Support), superiority::preference(Beta, A)), Weaker),
    member(Alpha, Weaker),
    \+ (member(Other, Weaker), Other \== Alpha, superiority::preference(Alpha, Other)),
    store::directAttack(undermine, D, Premise),
    store::claim(Premise, Alpha),
    assumptionsOf(D, Deduction),
    \+ member(Alpha, Deduction),
    contained(Deduction, [Beta|Support]), !.
contraposed(X, On) :-
    store::claim(On, Beta),
    assumptionsOf(X, Support),
    throw(error(
        domain_error(weak_contraposition, attack(Support, Beta)),
        'aba_plus: no weak contraposition (some assumptions attack a stronger one, and no argument from them and the stronger one attacks the weakest of them)'
    )).

%! reverse(+Type, +Attacker, +Attacked, +On)
%  The attack of Attacker on On, the argument for an assumption, reversed if Attacker rests on a weaker
%  assumption: On attacks every argument whose support contains the one of Attacker.
reverse(T, X, Y, On) :-
    restsOnWeaker(X, On), !,
    store::removeAttack(T, X, Y, On),
    assumptionsOf(X, Support),
    findall(_, (
        store::argument(Z),
        assumptionsOf(Z, Others),
        contained(Support, Others),
        store::saveAttack(reverse, On, Z, Z)
    ), _).
reverse(_, _, _, _).

% The attacker rests on an assumption weaker than the one On is the argument for
restsOnWeaker(Attacker, On) :-
    store::claim(On, Assumption),
    assumptionsOf(Attacker, Support),
    member(Weaker, Support),
    superiority::preference(Assumption, Weaker), !.

%! assumptionsOf(+Argument, -Assumptions)
%  The assumptions Argument rests on: those of its premises that are assumptions (see abaLanguage::assumption/2).
assumptionsOf(Argument, Assumptions) :-
    store::argumentRules(Argument, Rules),
    findall(A, (member(Id, Rules), abaLanguage::assumption(Id, A)), Assumptions).

contained([], _).
contained([A|As], Others) :- member(A, Others), !, contained(As, Others).

%==============================================================================
% QUERY MODE
%==============================================================================

%! queryDependencies(+Argument, -Statement)
%  An argument resting on an assumption weaker than another, Beta, may attack Beta through its contrary, and
%  have that attack reversed onto every argument resting on what it rests on, Argument among them: Argument
%  depends on the arguments for Beta and for its contraries.
queryDependencies(Argument, Statement) :-
    assumptionsOf(Argument, Support),
    member(Weaker, Support),
    superiority::preference(Beta, Weaker),
    (Statement = Beta ; contrary(Beta, Statement)).

%! queryDependants(+Argument, -Statement)
%  The argument for an assumption attacks, by a reversal, the arguments resting on the assumptions weaker than
%  it: those for the statements their rules conclude depend on it.
queryDependants(Argument, Statement) :-
    store::argumentRules(Argument, [Id]),
    abaLanguage::assumption(Id, Beta),
    superiority::preference(Beta, Weaker),
    (Statement = Weaker ; store::ruleUsing(Weaker, Statement)).

% Contrary is a contrary of the assumption Beta: the arguments for it attack the argument for Beta
contrary(Beta, Contrary) :-
    (conflict::guarded([Contrary], [Beta], _) ; conflict::unguarded([Contrary], [Beta])),
    nonvar(Contrary).
