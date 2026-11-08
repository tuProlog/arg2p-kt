package it.unibo.tuprolog.argumentation.core.modules.graph.extension

import it.unibo.tuprolog.argumentation.core.modules.PrologModule

/**
 * The preferences between the assumptions of ABA+ (module `abaPref`, a graphExtension, of the profile aba_plus;
 * Čyras and Toni 2016): `sup(Stronger, Weaker)`, closed transitively. An attack on an assumption by an argument
 * resting on an assumption weaker than it is reversed: it is removed, and the argument for the stronger assumption
 * attacks every argument whose support (its assumptions, see the module abaLanguage) contains the whole support of
 * the attacker. The reversal hits the arguments resting on the same assumptions as the attacker, not only the
 * attacker: as ABA+ reverses attacks between sets of assumptions, and the arguments do not share a sub-argument for
 * those sets. That holds on the theories satisfying Weak Contraposition (Čyras and Toni 2016, Axiom 4), and those
 * failing it are refused: their semantics are those of sets of assumptions, which the arguments do not capture.
 */
object AbaPrefModule : PrologModule("prolog.argumentation.graph.preferences.aba", "abaPref") {
    override val prologRawTheory: String =
        // language=Prolog
        """
        %! modifyArgumentationGraph
        %  The attacks on assumptions by arguments resting on weaker ones reversed, the theory satisfying Weak
        %  Contraposition.
        modifyArgumentationGraph :-
            graphStore::directAttacks(All),
            findall(attack(undermine, X, On, On), member((undermine, X, On), All), Attacks),
            utils::forEach(attack(_, X, _, On), Attacks, abaPref::contraposed(X, On)),
            utils::forEach(attack(T, X, Y, On), Attacks, abaPref::reverse(T, X, Y, On)).

        %! contraposed(+Attacker, +On)
        %  Weak Contraposition (Čyras and Toni 2016, Axiom 4), on which the reversal of the attacks between arguments
        %  is that of ABA+ between sets of assumptions: if Attacker, resting on the assumptions Support, attacks the
        %  assumption Beta though some are weaker than it, then for one Alpha of the weakest of them an argument
        %  attacks Alpha resting on Support but Alpha, and Beta. A theory failing it is refused.
        contraposed(X, On) :-
            \+ weaker(X, On), !.
        contraposed(X, On) :-
            argument::claim(On, Beta),
            support(X, Support),
            findall(A, (member(A, Support), theory::preference(Beta, A)), Weaker),
            member(Alpha, Weaker),
            \+ (member(Other, Weaker), Other \== Alpha, theory::preference(Alpha, Other)),
            graphStore::directAttack(undermine, D, Premise),
            argument::claim(Premise, Alpha),
            support(D, Deduction),
            \+ member(Alpha, Deduction),
            contained(Deduction, [Beta|Support]), !.
        contraposed(X, On) :-
            argument::claim(On, Beta),
            support(X, Support),
            throw(error(
                domain_error(weak_contraposition, attack(Support, Beta)),
                'aba_plus: no weak contraposition (some assumptions attack a stronger one, and no argument from them and the stronger one attacks the weakest of them)'
            )).

        %! reverse(+Type, +Attacker, +Attacked, +On)
        %  The attack of Attacker on On, the argument for an assumption, reversed if Attacker rests on a weaker
        %  assumption: On attacks every argument whose support contains the one of Attacker.
        reverse(T, X, Y, On) :-
            weaker(X, On), !,
            graphStore::removeAttack(T, X, Y, On),
            support(X, Support),
            findall(_, (
                graphStore::argument(Z),
                support(Z, Others),
                contained(Support, Others),
                graphStore::saveAttack(reverse, On, Z, Z)
            ), _).
        reverse(_, _, _, _).

        % The attacker rests on an assumption weaker than the one On is the argument for
        weaker(Attacker, On) :-
            argument::claim(On, Assumption),
            support(Attacker, Support),
            member(Weaker, Support),
            theory::preference(Assumption, Weaker), !.

        %! support(+Argument, -Assumptions)
        %  The assumptions Argument rests on: those of its premises asm(Assumption).
        support(Argument, Assumptions) :-
            argument::rules(Argument, Rules),
            findall(A, member(asm(A), Rules), Assumptions).

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
            support(Argument, Support),
            member(Weaker, Support),
            theory::preference(Beta, Weaker),
            (Statement = Beta ; contrary(Beta, Statement)).

        %! queryDependants(+Argument, -Statement)
        %  The argument for an assumption attacks, by a reversal, the arguments resting on the assumptions weaker than
        %  it: those for the statements their rules conclude depend on it.
        queryDependants(Argument, Statement) :-
            argument::rules(Argument, [asm(Beta)]),
            theory::preference(Beta, Weaker),
            (Statement = Weaker ; theory::ruleUsing(Weaker, Statement)).

        % Contrary is a contrary of the assumption Beta: the arguments for it attack the argument for Beta
        contrary(Beta, Contrary) :-
            (conflict::guarded([Contrary], [Beta], _) ; conflict::unguarded([Contrary], [Beta])),
            nonvar(Contrary).
        """.trimIndent()
}
