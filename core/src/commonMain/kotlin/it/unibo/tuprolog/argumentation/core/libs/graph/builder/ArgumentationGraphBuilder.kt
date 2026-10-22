package it.unibo.tuprolog.argumentation.core.libs.graph.builder

import it.unibo.tuprolog.argumentation.core.libs.PrologModule

/**
 * The whole argumentation graph of a theory (module `standard_af`, the graphBuildMode of the same name): every
 * argument its rules make, and the attacks between them, saved in the context (see the module graphStore).
 */
object ArgumentationGraphBuilder : PrologModule("prolog.argumentation.graph", "standard_af") {
    override val prologRawTheory: String =
        """
        %! buildArgumentationGraph
        %  The arguments of the theory, those derived from them, and the attacks between them.
        buildArgumentationGraph :-
            buildArguments,
            buildAdditionalArguments,
            buildAttacks.

        %! buildAdditionalArguments
        %  The arguments derived from the others (see the module conflictArguments).
        buildAdditionalArguments :-
            conflictArguments::buildAdditionalArguments.

        %--------------------------------------------------------------------------
        % Arguments
        %--------------------------------------------------------------------------

        %! buildArguments
        %  The arguments made of a premise, those of the rules with no premise to support, and then, round after round,
        %  those of the rules using a conclusion of the previous round.
        buildArguments :-
            buildArgumentsFromPremises,
            buildArgumentsFromEmptyRules,
            findall([RuleID, RuleBody, RuleHead], (
                parser::classic_rule(RuleID, RuleBody, RuleHead),
                \+ emptyRule([RuleID, RuleBody, RuleHead])
            ), Rules),
            context_retract(graphRule(_)),
            storeRules(Rules),
            findall(X, graphStore::newConclusion(a, [X]), N),
            buildArgumentsFromRules(N, b).

        % The rules are kept in the context rather than passed around: they are not ground, so carrying their list
        % in the goals makes every resolution step copy it. Asserted from the last, as context_check returns the
        % newest clauses first.
        storeRules([]).
        storeRules([Rule|Rules]) :-
            storeRules(Rules),
            context_assert(graphRule(Rule)).

        buildArgumentsFromPremises :-
            findall(_, (
                parser::classic_rule(PremiseID, Premise),
                defeasibleIds(PremiseID, DefeasiblePremises),
                argument::infoParts(Info, [], [], DefeasiblePremises),
                argument::new([PremiseID], none, Premise, [], Info, Argument),
                graphStore::saveArgument(a, Premise, Argument)
            ), _).

        buildArgumentsFromEmptyRules :-
            findall(_, (
                emptyRule([RuleID, RulePrem, RuleHead]),
                ruleBodyIsSupported(RulePrem, rule(RuleID, RuleHead, RulePrem), [[], [], [], []], _),
                defeasibleIds(RuleID, DefeasibleRules),
                argument::infoParts(Info, DefeasibleRules, DefeasibleRules, []),
                argument::new([RuleID], RuleID, RuleHead, RulePrem, Info, Argument),
                graphStore::saveArgument(a, RuleHead, Argument)
            ), _).

        % A rule with no premise to support with an argument: only contraries and Prolog checks
        emptyRule([RuleID, Body, RuleHead]) :-
            parser::classic_rule(RuleID, Body, RuleHead),
            \+ (member(X, Body), X \= ~(_), X \= prolog(_)).

        % The defeasible ones among the rule or premise Id: none if strict
        defeasibleIds(Id, []) :- parser::check_strict(Id), !.
        defeasibleIds(Id, [Id]).

        % A round fires every rule whose body uses a conclusion derived in the previous round (N), tagging the new
        % conclusions with Tn: the rounds go on while they derive something
        buildArgumentsFromRules(N, Tn) :-
            findall(_, (
                context_check(graphRule([Id, Body, Conc])),
                utils::contains_any(Body, N),
                buildArgumentsFromRule(Tn, [Id, Body, Conc])
            ), _),
            (graphStore::newConclusion(Tn, _) -> nextRound(Tn) ; true).

        buildArgumentsFromRule(Tn, [RuleID, RuleBody, RuleHead]) :-
            ruleBodyIsSupported(RuleBody, rule(RuleID, RuleHead, RuleBody), [[], [], [], []], Argument),
            graphStore::saveArgument(Tn, RuleHead, Argument).

        nextRound(Tn) :-
            otherTag(Tn, Tx),
            graphStore::forgetNewConclusions(Tx),
            findall(X, graphStore::newConclusion(Tn, [X]), N),
            buildArgumentsFromRules(N, Tx).

        otherTag(a, b).
        otherTag(b, a).

        ruleStrictness(Id, strict) :- parser::check_strict(Id), !.
        ruleStrictness(_, defeasible).

        % ruleBodyIsSupported(Body, rule(RuleID, RuleHead, RuleBody), [All, LastDefRules, DefRules, DefPremises], Argument):
        % Argument is built from RuleID by supporting each statement in Body with a saved argument, collecting the rules
        % and premises of the supports. Only Body is matched in the clause heads, everything else is unpacked in the
        % bodies: matching heads with many arguments against every clause is what makes this predicate expensive.
        ruleBodyIsSupported([], Rule, Acc, Argument) :-
            Rule = rule(RuleID, RuleHead, RuleBody),
            ruleStrictness(RuleID, Strict),
            argument::rulesAndInfo(RuleID, Strict, Acc, Rules, Info),
            argument::new(Rules, RuleID, RuleHead, RuleBody, Info, Argument),
            \+ graphStore::argumentFor(RuleHead, Argument).
        ruleBodyIsSupported([~(_)|Others], Rule, Acc, Argument) :-
            ruleBodyIsSupported(Others, Rule, Acc, Argument).
        ruleBodyIsSupported([prolog(Check)|Others], Rule, Acc, Argument) :-
            (callable(Check) -> call(Check); Check),
            ruleBodyIsSupported(Others, Rule, Acc, Argument).
        ruleBodyIsSupported([Statement|Others], Rule, Acc, Argument) :-
            Acc = [All, LastDef, Def, Prem],
            graphStore::argumentFor([Statement], Support),
            argument::rules(Support, Rules),
            argument::info(Support, Info),
            argument::infoParts(Info, SupportLastDef, SupportDef, SupportPrem),
            ruleBodyIsSupported(Others, Rule, [[Rules|All], [SupportLastDef|LastDef], [SupportDef|Def], [SupportPrem|Prem]], Argument),
            graphStore::saveSupport(Support, Argument).

        %--------------------------------------------------------------------------
        % Attacks
        %--------------------------------------------------------------------------

        %! buildAttacks
        %  The attacks between the saved arguments, direct and on their super-arguments. Loops over the graph go
        %  through its copy by identifiers (see the module graphStore).
        buildAttacks :-
            findall(_, buildDirectAttack, _).

        buildDirectAttack :-
            graphStore::argumentById(IdA, A),
            attackCandidates::possibleAttackers(IdA, A, Conflict),
            findAttacker(Conflict, B),
            conflictArguments::canAttack(B),
            A \== B,
            % Attacks are checked on copies, so that A and B are saved exactly as they are stored:
            % binding their variables would change their identifiers (see context_id)
            copy_term(B-A, CB-CA),
            attackRelation::attacks(T, CB, CA),
            graphStore::argumentId(B, IdB),
            graphStore::saveAttackById(T, IdB, B, IdA, A, IdA, A),
            buildTransitiveAttacks(T, IdB, B, IdA, A),
            fail.
        buildDirectAttack.

        % Finds the stored arguments whose conclusion unifies with the given one, without instantiating them
        findAttacker([Head], B) :- !,
            graphStore::argumentUnifiable([Head], B).
        findAttacker(Conclusion, B) :-
            graphStore::argumentFor(Conclusion, B).

        %! buildTransitiveAttacks(+Type, +IdB, +B, +IdA, +A)
        %  B, attacking A, attacks every argument that A supports, directly or not.
        buildTransitiveAttacks(T, IdB, B, IdA, A) :-
            findall(_, (
                graphStore::supportedById(IdA, IdC),
                graphStore::argumentById(IdC, C),
                graphStore::saveAttackById(T, IdB, B, IdC, C, IdA, A)
            ),_).

        """.trimIndent()
}
