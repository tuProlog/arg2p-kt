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
        // language=Prolog
        """
        %! modifyArgumentationGraph
        %  The attacks the preferences may invalidate, as arguments, attacked by the arguments for the preferences, each
        %  on its own (see modifyArgumentationGraph/1).
        modifyArgumentationGraph :-
            modifyArgumentationGraph(each).

        %! modifyArgumentationGraph(+Model)
        %  As modifyArgumentationGraph/0, the attack arguments attacked by an argument for each preference invalidating
        %  them (Model each), or for all of them together, preference(Preferences) (Model all, the extension
        %  defeasibleAllPref).
        modifyArgumentationGraph(Model) :-
            assertAllSup,
            graphStore::attacks(Attacks),
            filterSupRelatedAttacks(Attacks, InvalidAttacks),
            convertAttacks(InvalidAttacks),
            buildPrefAttacks(Model).

        %! assertAllSup
        %  The preferences concluded by the arguments, as the preferences (see parser::statePreferences/1), in place of
        %  those the theory states: only the concluded ones count.
        assertAllSup :-
            findall(sup(RuleOne, RuleTwo), (
                argument::claim(Argument, sup(RuleOne, RuleTwo)),
                graphStore::argument(Argument)
            ), Concluded),
            utils::deduplicate(Concluded, Preferences),
            parser::statePreferences(Preferences).

        %! filterSupRelatedAttacks(+Attacks, -Invalid)
        %  Invalid are the attacks among Attacks that the preferences invalidate. Those invalid whatever the
        %  preferences (C superior to A with none) are removed, as standardPref does: no argument could save them.
        filterSupRelatedAttacks([], []).
        filterSupRelatedAttacks([(T, A, B, C)|Attacks], Invalid) :-
            invalid(T, A, B, C, []), !,
            graphStore::removeAttack(T, A, B, C),
            filterSupRelatedAttacks(Attacks, Invalid).
        filterSupRelatedAttacks([(T, A, B, C)|Attacks], [(T, A, B, C)|Invalid]) :-
            invalid(T, A, B, C, _), !,
            filterSupRelatedAttacks(Attacks, Invalid).
        filterSupRelatedAttacks([_|Attacks], Invalid) :-
            filterSupRelatedAttacks(Attacks, Invalid).

        %! invalid(+Type, +A, +B, +C, -Preferences)
        %  The attack of A on B (on C) is invalidated by the preferences Preferences, C being superior to A: each
        %  justification of the superiority in turn (see superiority::superiorArgument/3).
        invalid(rebut, A, _, C, Preferences) :- superiority::superiorArgument(C, A, Preferences).
        invalid(undermine, A, _, C, Preferences) :- superiority::superiorArgument(C, A, Preferences).

        %! convertAttacks(+Attacks)
        %  Each attack becomes an argument for it: an attack of A on B becomes the argument X for attack(T, A, B, B),
        %  supported by A and attacking B. If A attacks B through C, a sub-argument of B, the argument for the attack
        %  on C attacks B, as the attacks on C extend to the arguments built on it (see the module graphStore).
        convertAttacks(Attacks) :-
            conversion(Attacks).

        conversion(List) :-
            member((T, A, B, B), List),
            generateDirectAttackArgument(T, A, B, B),
            fail.
        conversion(_).

        % The attacks on A extend to the argument for the attack, which A supports
        generateDirectAttackArgument(T, A, B, B) :-
            argument::infoParts(Info, [attack], [attack], []),
            argument::new([attack], attack, [attack(T, A, B, B)], [], Info, AttackArgument),
            graphStore::saveArgument(a, [attack(T, A, B, B)], AttackArgument),
            graphStore::saveSupport(A, AttackArgument),
            graphStore::saveAttack(T, AttackArgument, B, B),
            graphStore::removeAttack(T, A, B, B).

        %! attackArgument(?AttackArgument, ?Type, ?A, ?B, ?C)
        %  AttackArgument is the saved argument for the attack attack(Type, A, B, C).
        attackArgument(AttackArgument, Type, A, B, C) :-
            argument::topRule(AttackArgument, attack),
            argument::claim(AttackArgument, attack(Type, A, B, C)),
            graphStore::argument(AttackArgument).

        % The arguments for the preferences invalidating an attack, by any of its justifications, attack its argument
        % (and so what it supports): each of them (each), or one for all of them together (all), for each choice of
        % an argument for each preference (see argumentsFor/2), each standing or falling with its own
        buildPrefAttacks(each) :-
            attackArgument(Attack, T, A, B, C),
            invalid(T, A, B, C, Preferences),
            member(X, Preferences),
            argument::claim(Preference, X),
            graphStore::argument(Preference),
            graphStore::saveAttack(pref, Preference, Attack, Attack),
            fail.
        buildPrefAttacks(all) :-
            attackArgument(Attack, T, A, B, C),
            invalid(T, A, B, C, Preferences),
            argumentsFor(Preferences, Supports),
            createSuperiorityArgument(Preferences, Supports, Argument),
            graphStore::saveAttack(pref, Argument, Attack, Attack),
            fail.
        buildPrefAttacks(_).

        % Supports is each choice of one argument for each of Preferences, in their order
        argumentsFor([], []).
        argumentsFor([X|Xs], [Support|Supports]) :-
            argument::claim(Support, X),
            graphStore::argument(Support),
            argumentsFor(Xs, Supports).

        % The argument for preference(Preferences) supported by Supports, one argument for each of them, and so attacked
        % as they are (see the module graphStore): saved once, for the first attack the preferences invalidate
        createSuperiorityArgument(Preferences, Supports, Argument) :-
            mergeIds(Supports, MergedId),
            argument::infoParts(Info, [pref], MergedId, []),
            argument::new(MergedId, pref, [preference(Preferences)], [], Info, Argument),
            (graphStore::saveArgument(a, [preference(Preferences)], Argument) -> saveRelations(Supports, Argument) ; true).

        saveRelations(Supports, Argument) :-
            findall(_, (
                member(A, Supports),
                graphStore::saveSupport(A, Argument)
            ), _).

        % The rules of the arguments, after pref
        mergeIds([], [pref]).
        mergeIds(Arguments, [pref|MergedId]) :-
            findall(Rules, (member(A, Arguments), argument::rules(A, Rules)), Res),
            utils::appendLists(Res, MergedId).

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

        ruleConclusion(Rule, S) :- parser::classicRule(Rule, _, [S]).
        ruleConclusion(Rule, S) :- parser::classicRule(Rule, [S]).
        """.trimIndent()
}
