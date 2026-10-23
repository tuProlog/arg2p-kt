package it.unibo.tuprolog.argumentation.core.modules.graph.extension

import it.unibo.tuprolog.argumentation.core.modules.PrologModule

/**
 * The preferences between rules as conclusions of the theory (module `defeasiblePref`, a graphExtension): an
 * argument for `sup(Rule, Other)` makes the attacks that preference invalidates fail, as long as it stands.
 *
 * Each attack it may invalidate becomes an argument for `attack(Type, Attacker, Attacked, On)` (top rule `attack`),
 * supported by the attacker and attacking in its place; the arguments for the preferences invalidating it attack it
 * (attacks of type `pref`).
 */
object DefeasiblePrefModule : PrologModule("prolog.argumentation.graph.preferences.defeasible", "defeasiblePref") {
    override val prologRawTheory: String =
        """
        %! modifyArgumentationGraph
        %  The attacks the preferences may invalidate, as arguments, attacked by the arguments for the preferences.
        modifyArgumentationGraph :-
            assertAllSup,
            graphStore::attacks(Attacks),
            filterSupRelatedAttacks(Attacks, InvalidAttacks),
            convertAttacks(InvalidAttacks),
            buildPrefAttacks.

        %! assertAllSup
        %  The preferences concluded by the arguments, as sup/2 facts of the context (see the module superiority).
        assertAllSup :-
            context_retract(sup(_, _)),
            findall(_, (
                argument::claim(Argument, sup(RuleOne, RuleTwo)),
                graphStore::argument(Argument),
                context_absent(sup(RuleOne, RuleTwo)),
                context_assert(sup(RuleOne, RuleTwo))
            ), _).

        %! filterSupRelatedAttacks(+Attacks, -Invalid)
        %  Invalid are the attacks among Attacks that the preferences invalidate.
        filterSupRelatedAttacks([], []).
        filterSupRelatedAttacks([(T, A, B, C)|Attacks], [(T, A, B, C)|Invalid]) :-
            invalid(T, A, B, C, _),
            filterSupRelatedAttacks(Attacks, Invalid), !.
        filterSupRelatedAttacks([_|Attacks], Invalid) :-
            filterSupRelatedAttacks(Attacks, Invalid).

        %! invalid(+Type, +A, +B, +C, -Preferences)
        %  The attack of A on B (on C) is invalidated by the preferences Preferences, C being superior to A.
        invalid(rebut, A, B, C, Preferences) :- superiority::superiorArgument(C, A, Preferences), !.
        invalid(undermine, A, B, C, Preferences) :- superiority::superiorArgument(C, A, Preferences), !.

        %! convertAttacks(+Attacks)
        %  Each attack becomes an argument for it: an attack of A on B becomes the argument X for attack(T, A, B, B),
        %  supported by A and attacking B. If A attacks B through C, a sub-argument of B, the argument for the attack
        %  on C attacks B (transitive attack).
        convertAttacks(Attacks) :-
            conversion(Attacks).

        conversion(List) :-
            member((T, A, B, B), List),
            generateDirectAttackArgument(T, A, B, B),
            fail.
        conversion(List) :-
            member((T, A, B, C), List),
            C \= B,
            generateTransitiveAttackArgument(T, A, B, C),
            fail.
        conversion(_).

        generateTransitiveAttackArgument(T, A, B, C) :-
            attackArgument(AttackArgument, T, A, C, C),
            graphStore::saveAttack(T, AttackArgument, B, C),
            graphStore::removeAttack(T, A, B, C).

        generateDirectAttackArgument(T, A, B, B) :-
            argument::infoParts(Info, [attack], [attack], []),
            argument::new([attack], attack, [attack(T, A, B, B)], [], Info, AttackArgument),
            graphStore::saveArgument(a, [attack(T, A, B, B)], AttackArgument),
            graphStore::saveSupport(A, AttackArgument),
            findall(_, (
                graphStore::attack(X, Y, A, Z),
                graphStore::saveAttack(X, Y, AttackArgument, Z)
            ), _),
            graphStore::saveAttack(T, AttackArgument, B, B),
            graphStore::removeAttack(T, A, B, B).

        %! attackArgument(?AttackArgument, ?Type, ?A, ?B, ?C)
        %  AttackArgument is the saved argument for the attack attack(Type, A, B, C).
        attackArgument(AttackArgument, Type, A, B, C) :-
            argument::topRule(AttackArgument, attack),
            argument::claim(AttackArgument, attack(Type, A, B, C)),
            graphStore::argument(AttackArgument).

        %! buildPrefAttacks
        %  The arguments for the preferences invalidating an attack attack its argument, and what it supports.
        buildPrefAttacks :-
            attackArgument(Attack, T, A, B, C),
            invalid(T, A, B, C, Preferences),
            member(X, Preferences),
            argument::claim(Preference, X),
            graphStore::argument(Preference),
            graphStore::saveAttack(pref, Preference, Attack, Attack),
            prefersTransitively(Preference, Attack),
            fail.
        buildPrefAttacks.

        %! prefersTransitively(+Preference, +Attack)
        %  The preference also attacks the arguments the attack argument supports.
        prefersTransitively(Preference, Attack) :-
            graphStore::argumentId(Preference, IdP),
            graphStore::argumentId(Attack, IdA),
            standard_af::buildTransitiveAttacks(pref, IdP, Preference, IdA, Attack).

        %==============================================================================
        % QUERY MODE
        %==============================================================================

        %! queryDependencies(+Argument, -Statement)
        %  The preferences between the rules of Argument are themselves conclusions (sup/2).
        queryDependencies(Argument, S) :-
            argument::rules(Argument, Rules),
            member(Rule, Rules),
            (S = sup(Rule, _) ; S = sup(_, Rule)).

        %! queryDependants(+Argument, -Statement)
        %  An argument for a preference relates the arguments built on its rules.
        queryDependants(Argument, S) :-
            argument::claim(Argument, sup(X, Y)),
            (Rule = X ; Rule = Y),
            ruleConclusion(Rule, S).

        ruleConclusion(Rule, S) :- parser::classic_rule(Rule, _, [S]).
        ruleConclusion(Rule, S) :- parser::classic_rule(Rule, [S]).
        """.trimIndent()
}
