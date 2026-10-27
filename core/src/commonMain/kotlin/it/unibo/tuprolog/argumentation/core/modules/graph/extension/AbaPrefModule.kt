package it.unibo.tuprolog.argumentation.core.modules.graph.extension

import it.unibo.tuprolog.argumentation.core.modules.PrologModule

/**
 * The preferences between the assumptions of ABA+ (module `abaPref`, a graphExtension, of the profile aba_plus;
 * Čyras and Toni 2016): `sup(Stronger, Weaker)`, closed transitively. An attack on an assumption by an argument
 * resting on an assumption weaker than it is reversed: it is removed, and the argument for the stronger assumption
 * attacks every argument whose support (its assumptions, see the module abaLanguage) contains the whole support of
 * the attacker. The reversal hits the arguments resting on the same assumptions as the attacker, not only the
 * attacker: as ABA+ reverses attacks between sets of assumptions, and the arguments do not share a sub-argument for
 * those sets.
 */
object AbaPrefModule : PrologModule("prolog.argumentation.graph.preferences.aba", "abaPref") {
    override val prologRawTheory: String =
        // language=Prolog
        """
        %! modifyArgumentationGraph
        %  The attacks on assumptions by arguments resting on weaker ones reversed.
        modifyArgumentationGraph :-
            findall(attack(undermine, X, Y, On), graphStore::attack(undermine, X, Y, On), Attacks),
            utils::forEach(attack(T, X, Y, On), Attacks, abaPref::reverse(T, X, Y, On)).

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
            context_check(sup(Assumption, Weaker)), !.

        %! support(+Argument, -Assumptions)
        %  The assumptions Argument rests on: those of its premises asm(Assumption).
        support(Argument, Assumptions) :-
            argument::rules(Argument, Rules),
            findall(A, member(asm(A), Rules), Assumptions).

        contained([], _).
        contained([A|As], Others) :- member(A, Others), !, contained(As, Others).
        """.trimIndent()
}
